require "rly"

def reload!
  Tablegen.send(:remove_const, :Parser)
  Tablegen.send(:remove_const, :Lexer)
  load 'tablegen-rb/parser.rb'
end

module Tablegen
  class Lexer < Rly::FileLex
    attr_accessor :linepos

    def initialize(fn)
      super
      @lineno = 1
    end

    ignore " \t"

    literals "<>{};=,:[]-"

    token(/\n+/) do |t|
      t.lexer.lineno += t.value.length
      t.lexer.linepos = 1
      nil
    end

    token(/\/\/[^\n]*/)

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

    token :ID, /[a-zA-Z_][a-zA-Z0-9_]*/
    token :STRING, /"[^"]*"/
    token :NUMBER, /[0-9]+/

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

      @filename = fn
      begin
        @input = open(fn).read
        @pos = 0
      rescue
        puts "Lexer: failed to read #{fn}, will pop"
        pop_file
      end
    end

    def pop_file
      (@input, @pos, @filename, @lineno) = @inputstack.pop
    end
  end

  class Parser < Rly::Yacc
    def self.run(i, t=false)
      p = Parser.new(Lexer.new("spec/fixtures/#{i}.td"))
      p.parse('', t)
    end

    rule 'tabledef : definitions' do |tf, df|
      tf.value = df.value
    end

    rule 'definitions : definition
                      | definition definitions' do |defs, df, other_defs|
      defs.value = [df.value]
      defs.value += other_defs.value if other_defs
    end

    rule 'definition : classdef' do |df, clsdef|
      df.value = clsdef.value
    end

    rule 'definition : defdef' do |df, dfdef|
      df.value = dfdef.value
    end

    rule 'definition : letdef' do |df, letdef|
      df.value = letdef.value
    end

    rule 'defdefs : defdef
                  | defdef defdefs' do |defs, df, other_dfs|
      defs.value = [df.value]
      defs.value += other_dfs.value if other_dfs
    end

    # classes
    rule 'classdef : CLASS ID maybe_classdefargs maybe_superclass maybe_classdefbody maybe_colon' do |cdef, _, name, args, sup, body, _|
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
        args.value = rargs
      else
        args.value = []
      end
    end

    rule 'maybe_classdefbody : "{" classdefbody "}" |' do |body, _, rbody, _|
      if rbody
        body.value = rbody
      else
        body.value = []
      end
    end

    rule 'maybe_superclass : ":" ID
                           | ":" ID "<" values ">"
                           |' do |sup, _, name, _, args|
      if name
        sup.value = { name: name.value }
        sup.value[:args] = args.value if args
      else
        sup.value = nil
      end
    end

    rule 'maybe_colon : ";" |' do ; end

    rule 'classdefargs : typename_maybedef
                       | typename_maybedef "," classdefargs' do |args, tn, _, other_args|
      args.value = [tn.value]
      args.value += other_args.value if other_args
    end

    rule 'classdefbody : classfield
                       | classfield classdefbody' do |body, f, other_fields|
      body.value = [f.value]
      body.value += other_fields.value if other_fields
    end

    rule 'classfield : typevalueassign ";"' do |f, va, _|
      f.value = va.value
    end

    rule 'typevalueassign : type valueassign' do |tva, t, va|
      tva.value = {
        type: t.value,
        name: va.value[:name],
        value: va.value[:value],
      }
    end

    rule 'valueassign : ID "=" value' do |va, n, _, v|
      va.value = {
        name: n.value,
        value: v.value,
      }
    end

    rule 'typename_maybedef : typename "=" value
                            | typename' do |tnd, tn, _, val|
      tnd.value = tn.value
      tnd.value[:value] = val.value if val
    end

    rule 'typename : type ID' do |tn, t, n|
      tn.value = { type: t.value, name: n.value }
    end

    rule 'type : ID
               | ID "<" type ">"' do |ty, name, _, other_ty|
      if other_ty
        ty.value = { name: name.value, sub: other_ty.value }
      else
        ty.value = name.value
      end
    end

    # defs
    rule 'defdef : DEF ID ":" ID ";"' do |df, _, name, _, cname, _|
      df.value = {
        kind: :def,
        name: name.value,
        classname: cname.value,
        def_at: name.location_info,
      }
    end

    rule 'defdef : DEF ID ":" ID "<" values ">" ";"
                 | DEF ID ":" ID "<" values ">" "," values ";" ' do |df, _, name, _, cname, _, vals, _, _, randomval, _|
      df.value = {
        kind: :def,
        name: name.value,
        vals: vals.value,
        classname: cname.value,
        def_at: name.location_info,
      }
      df.value[:randomval] = randomval.value if randomval
    end

    rule 'values : value
                 | value "," values' do |vals, v, _, other_vals|
      vals.value = [v.value]
      vals.value += other_vals.value if other_vals
    end

    rule 'value : ID
                | id_anglevalues
                | STRING
                | NUMBER
                | array' do |v, the_v|
      v.value = the_v.value
    end

    rule 'id_anglevalues : ID "<" values ">"' do |i, name, _, vals|
      i.value = { name: name, params: vals.value }
    end

    rule 'array : "[" values "]"
                | "[" "]"' do |ar, _, vals|
      ar.value = vals ? vals.value : []
    end

    rule 'letdef : LET valueassign IN "{" defdefs "}"' do |ld, _, va, _, _, defs, _|
      ld.value = {
        kind: :let,
        let_value: va.value,
        in: defs.value,
      }
    end

    on_error -> tok {
      puts "Error at #{tok.location_info}"
      puts "Stack: #{@symstack.map {|s| s.kind_of?(Rly::LexToken) ? s.inspect : nil}.compact.join(', ')}  <-- #{tok.inspect}"
      `subl #{tok.location_info[:filename]}:#{tok.location_info[:lineno]}:#{tok.location_info[:pos]}`
      raise RuntimeError
    }
  end
end
