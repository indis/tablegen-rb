require "rly"
require "ruby-prof"

def reload!
  Tablegen.send(:remove_const, :Parser)
  Tablegen.send(:remove_const, :Lexer)
  load 'tablegen-rb/parser.rb'
end

def run!(i, t=false)
  l = Tablegen::Lexer.new()
  l.search_paths = ['spec/fixtures']
  l.push_file("#{i}.td")
  p = Tablegen::Parser.new(l)
  p.parse('', t)
end

def run_prof!(i='llvm/Intrinsics')
  result = RubyProf.profile { run!(i) }
  printer = RubyProf::CallStackPrinter.new(result)
  File.open('prof/prof.html', 'w') { |f| printer.print(f, :threshold => 0, :min_percent => 0) }
  `open prof/prof.html`
end

module Tablegen
  class Lexer < Rly::FileLex
    attr_accessor :linepos, :search_paths

    def initialize(fn=nil)
      super
      @lineno = 1
    end

    metatokens :CODE

    ignore " \t"

    literals "<>{};=,:[]-().?"

    token(/\n+/) do |t|
      t.lexer.lineno += t.value.length
      t.lexer.linepos = 1
      nil
    end

    token(/\/\/[^\n]*/)
    token(/\/\*[^\*]*\*\//)

    token(/include\s+"[^"]+"/) do |t|
      /include\s+"([^"]+)"/ =~ t.value
      t.lexer.push_file($~[1])
      t.lexer.lineno = 1
      nil
    end

    token :LET, /let\s/
    token :IN, /in\s/
    token :CLASS, /class\s/
    token :DEF, /def\s/
    token :DEFM, /defm\s/
    token :MULTICLASS, /multiclass\s/
    token :FIELD, /field\s/

    token :CODEOPEN, /\[\{/
    token :CODECLOSE, /\}\]/

    token :FUNCNAME, /![a-zA-Z_][a-zA-Z0-9_]*/
    token :RID, /\$[a-zA-Z_][a-zA-Z0-9_]*/
    token :ID, /[a-zA-Z_][a-zA-Z0-9_]*/
    token :STRING, /"[^"]*"/
    token :HEXNUMBER, /0x[0-9a-fA-F]+/ do |t|
      t.value = t.value.to_i(16)
      t.type = :NUMBER
      t
    end
    token :BITNUMBER, /0b[0-9]+/ do |t|
      t.value = t.value.to_i(2)
      t.type = :NUMBER
      t
    end
    token :RANGE, /\d+\s*\-\s*\d+/ do |t|
      r1,r2 = t.value.split('-')
      t.value = r1..r2
      t
    end
    token :NUMBER, /\-?[0-9]+/ do |t|
      t.value = t.value.to_i
      t
    end

    def build_token(type, value)
      tok = super
      tok.location_info[:pos] = self.linepos
      self.linepos += value.length
      tok
    end

    def linepos
      @linepos ||= 1
    end

    def ignore_symbol
      super
      @linepos += 1
    end

    def push_file(fn)
      @inputstack.push([@input, @pos, @filename, @lineno]) if @filename

      search_paths = self.search_paths.dup
      begin
        @filename = fn
        @input = open(fn).read
        @pos = 0
      rescue Errno::ENOENT
        unless search_paths.empty?
          p = search_paths.shift
          fn = File.join(p, fn)
          retry
        else
          raise RuntimeError.new("Lexer: failed to read #{fn}")
          puts "Lexer: failed to read #{fn}, will pop"
          pop_file
        end
      end
    end

    def pop_file
      fn = @filename
      (@input, @pos, @filename, @lineno) = @inputstack.pop
    end

    def search_paths
      @search_paths ||= []
    end
  end

  class Parser < Rly::Yacc
    # store_grammar 'rules.out'

    rule 'tabledef : definitions' do |tf, df|
      tf.value = df.value
    end

    rule 'definitions : definition
                      | definition definitions' do |defs, df, _, other_defs|
      defs.value = [df.value]
      defs.value += other_defs.value if other_defs
    end

    rule 'definition : classdef' do |df, clsdef|
      df.value = clsdef.value
    end

    rule 'definition : defdef maybe_semicolon' do |df, dfdef|
      df.value = dfdef.value
    end

    rule 'definition : defmdef' do |df, dfmdef|
      df.value = dfmdef.value
    end

    rule 'definition : letvalueassign ";"' do |df, letdef|
      df.value = letdef.value
    end

    rule 'definition : letdef' do |df, letdef|
      df.value = letdef.value
    end

    rule 'definition : multiclassdef' do |df, mcdef|
      df.value = mcdef.value
    end

    # classes
    rule 'classdef : CLASS ID maybe_classdefargs maybe_superclasses maybe_classdefbody maybe_semicolon' do |cdef, _, name, args, sup, body, _|
      cdef.value = {
        kind: :class,
        name: name.value,
        args: args.value,
        body: body.value,
        superclass: sup.value,
        def_at: name.location_info,
      }
    end

    rule 'maybe_classdefargs : "<" classdefargs ">" |' do |args, _, rargs, _|
      if rargs
        args.value = rargs.value
      else
        args.value = []
      end
    end

    rule 'maybe_classdefbody : "{" classdefbody "}" |' do |body, _, rbody, _|
      if rbody
        body.value = rbody.value
      else
        body.value = []
      end
    end

    rule 'maybe_superclasses : ":" superclasses |' do |scs, _, other_scs|
      if other_scs
        scs.value = other_scs.value
      else
        scs.value = []
      end
    end

    rule 'superclasses : superclass
                       | superclass "," superclasses' do |scs, sc, _, other_scs|
      scs.value = [sc.value]
      scs.value += other_scs.value if other_scs
    end

    rule 'superclass : ID
                     | ID "<" values ">"
                     |' do |sup, name, _, args|
      if name
        sup.value = { name: name.value }
        sup.value[:args] = args.value if args
      else
        sup.value = nil
      end
    end

    rule 'maybe_semicolon : ";" |' do ; end

    rule 'classdefargs : typename_maybedef
                       | typename_maybedef "," classdefargs' do |args, tn, _, other_args|
      args.value = [tn.value]
      args.value += other_args.value if other_args
    end

    rule 'classdefbody : classfield
                       | classfield classdefbody
                       |' do |body, f, other_fields|
      if f
        body.value = [f.value]
        body.value += other_fields.value if other_fields
      else
        body.value = []
      end
    end

    rule 'classfield : typevalueassign ";"
                     | letvalueassign ";"' do |f, va, _|
      f.value = va.value
    end

    rule 'multiclassdef : MULTICLASS ID maybe_classdefargs "{" definitions "}"' do |cdef, _, name, args, _, defs, _|
      cdef.value = {
        kind: :multiclass,
        name: name.value,
        args: args.value,
        defs: defs.value,
        def_at: name.location_info,
      }
    end

    rule 'letvalueassign : LET valueassign' do |lva, _, va|
      lva.value = {
        kind: :let,
        name: va.value[:name],
        value: va.value[:value],
      }
    end

    rule 'typevalueassign : typename_maybedef' do |tva, t|
      tva.value = {
        type: t.value,
        name: t.value[:name],
        value: t.value[:value],
      }
    end

    rule 'valueassign : ID maybe_range "=" value' do |va, n, mr, _, v|
      va.value = {
        name: n.value,
        value: v.value,
      }
      va.value[:range] = mr.value if mr.value
    end

    rule 'maybe_range : "{" RANGE "}"
                      | "{" NUMBER "}"
                      |' do |mr, _, r, _|
      mr.value = r.value if r
    end

    rule 'typename_maybedef : typename "=" value
                            | typename' do |tnd, tn, _, val|
      tnd.value = tn.value
      tnd.value[:value] = val.value if val
    end

    rule 'typename : type ID
                   | fieldstype ID' do |tn, t, n|
      tn.value = { type: t.value, name: n.value }
    end

    rule 'fieldstype : FIELD type' do |ft, _, t|
      ft.value = t.value
    end

    rule 'type : ID
               | ID "<" type ">"
               | ID "<" NUMBER ">"' do |ty, name, _, other_ty|
      if other_ty
        ty.value = { name: name.value, sub: other_ty.value }
      else
        ty.value = name.value
      end
    end

    # mdefs
    rule 'defmdef : DEFM ID maybe_superclasses ";"' do |df, _, name, cname, _|
      df.value = {
        kind: :defm,
        name: name.value,
        classname: cname.value,
        def_at: name.location_info,
      }
    end

    # defs
    rule 'maybe_id : ID |' do |mid, i|
      mid.value = i.value if i
    end

    rule 'defdef : DEF maybe_id maybe_superclasses maybe_defbody' do |df, td, name, cname, _|
      df.value = {
        kind: :def,
        name: name.value,
        classname: cname.value,
        def_at: td.location_info,
      }
    end

    rule 'defdef : DEF maybe_id maybe_superclasses "<" values ">" ";"
                 | DEF maybe_id maybe_superclasses "<" values ">" "," values ";"' do |df, td, name, cname, _, vals, _, _, randomval, _|
      df.value = {
        kind: :def,
        name: name.value,
        vals: vals.value,
        classname: cname.value,
        def_at: td.location_info,
      }
      df.value[:randomval] = randomval.value if randomval
    end

    rule 'maybe_defbody : "{" bodydefs "}" |' do |db, _, df, _|
      db.value = df.value.compact if df
    end

    rule 'bodydefs : bodydef
                   | bodydef bodydefs' do |bdfs, d, _, other_bdfs|
      bdfs.value = [d.value]
      bdfs.value += other_bdfs.value if other_bdfs
    end

    rule 'bodydef : typevalueassign ";"
                  | letvalueassign ";"' do |bd, d|
      bd.value = d.value
    end

    rule 'values : value
                 | value "," values' do |vals, v, _, other_vals|
      vals.value = [v.value]
      vals.value += other_vals.value if other_vals
    end

    rule 'maybe_values : values |' do |vals, v|
      if v
        vals.value = v.value
      else
        vals.value = []
      end
    end

    rule 'value : id_anglevalues
                | string
                | NUMBER
                | array
                | dag
                | funccall
                | propvalue
                | bitarray
                | codeblock
                | ID maybe_range
                | refvalue
                | "?"' do |v, the_v, maybe_range| # TODO: bitarry is broken by codeblock
      v.value = the_v.value
      v.value = { val: v.value, range: maybe_range.value } if maybe_range
    end

    rule 'string : STRING
                 | STRING STRING' do |s, s1, s2|
      s.value = s1.value
      s.value += s2.value if s2
    end

    rule 'funccall : FUNCNAME maybe_cast "(" values ")"' do |fn, name, cast, _, args, _|
      fn.value = {
        kind: :func,
        name: name.value,
        args: args.value,
      }
      fn.value[:cast] = cast.value if cast.value
    end

    rule 'maybe_cast : "<" values ">" |' do |mc, _, val, _|
      mc.value = val.value if val
    end

    rule 'propvalue : ID "." value' do |pv, i, _, v|
      pv.value = {
        kind: :prop,
        name: i.value,
        value: v.value,
      }
    end

    rule 'refvalue : ID ":" RID' do |pv, i, _, v|
      pv.value = {
        kind: :ref,
        name: i.value,
        value: v.value,
      }
    end

    rule 'id_anglevalues : ID "<" values ">"' do |i, name, _, vals|
      i.value = { name: name, params: vals.value }
    end

    rule 'array : "[" values "]"
                | "[" "]"' do |ar, _, vals|
      ar.value = vals ? vals.value : []
    end

    rule 'dag : "(" value maybe_values ")"
                 | "(" ")"' do |da, _, nm, vals|
      da.value = vals ? vals.value : []
      da.value.insert(0, nm)
    end

    rule 'bitarray : "{" bitvalues "}"
                   | "{" "}"' do |ba, _, v, _|
      ba.value = v ? v.value : []
    end

    rule 'bitvalues : bitvalue
                    | bitvalue "," bitvalues' do |bv, v, _, other_bvs|
      bv.value = [v.value]
      bv.value += other_bvs.value if other_bvs
    end

    rule 'bitvalue : NUMBER
                   | ID
                   | "?"' do |bv, v|
      bv.value = v.value
    end

    rule 'codeblock : CODEOPEN CODE CODECLOSE
                    | CODEOPEN CODECLOSE' do |cb, _, c, _|
      cb.value = c.value if c
    end

    rule 'letdef : LET valueassigns IN definition
                 | LET valueassigns IN "{" definitions "}"' do |ld, _, va, _, defs1, defs2, _|
      if defs1 == '='
        ld.value = {
          kind: :let,
          let_value: va.value,
          in: defs2.value,
        }
      else
        ld.value = {
          kind: :let,
          let_value: va.value,
          in: defs1.value,
        }
      end
    end

    rule 'valueassigns : valueassign
                       | valueassign "," valueassigns' do |vals, v, _, other_vals|
      vals.value = [v.value]
      vals.value += other_vals.value if other_vals
    end

    def errors_found
      @errors_found ||= 0
    end

    attr_writer :errors_found

    def error_log(tok, final=false)
      puts "Error at #{tok.location_info}"

      syms = @symstack.dup.compact
      sh = syms.shift
      sh = syms.shift while sh.kind_of?(Rly::YaccSymbol)
      syms.insert(0, sh)
      syms = syms.map do |s|
        "\t" + s.inspect
      end

      puts "Stack:  \n#{syms.join("\n")}\n\t^^^\n\t#{tok.inspect}"

      `subl #{tok.location_info[:filename]}:#{tok.location_info[:lineno]}:#{tok.location_info[:pos]}`
      raise RuntimeError if final
    end

    on_error -> tok do
      prev_sym = @symstack[-1]
      prev_st = @statestack[-1]
      
      if prev_sym.type == :CODEOPEN && @lr_table.lr_action[@statestack[-1]].keys == [:CODE, :CODECLOSE]
        tok = @lex.instance_eval {
          p = @pos

          depth = 0
          while true
            @pos += 1
            @linepos += 1

            raise RuntimeError.new("End reached at #{@pos}?..") if @pos == @input.length

            case @input[@pos]
            when '{'
              depth += 1
            when '}'
              depth -= 1
            when "\n"
              @lineno += 1
            end

            break if depth == -1
          end

          # puts "Lexer: skipping over '#{@input[p...@pos]}'"
          build_token(:CODE, @input[p...@pos])
        }

        # tok = @lex.next
        @errorok = true
        return tok
      else
        error_log(tok, true)
      end
    end
  end
end
