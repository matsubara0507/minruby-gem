require "pp"
require "ripper"

class MinRubyParser
  def self.minruby_parse(program)
    MinRubyParser.new.minruby_parse(program)
  end

  def minruby_parse(program)
    simplify(Ripper.sexp(program))
  end

  def simplify(exp)
    case exp
    in (:program | :bodystmt), exp1, *_
      make_stmts(exp1)
    in :def, [_, name, _], params, body
      params = params[1] if params[0] == :paren
      params = (params[1] || []).map {|a| a[1] }
      ["func_def", name, params, simplify(body)]
    in :call, recv, _, [_, name, _]
      ["method_call", simplify(recv), name, []]
    in :fcall, [_, name, _]
      ["func_call", name]
    in :method_add_arg, exp1, exp2
      call = simplify(exp1)
      e = exp2
      e = e[1] || [] if e[0] == :arg_paren
      e = e[1] || [] if e[0] == :args_add_block
      e = e.map {|e_| simplify(e_) }
      call[(call[0] == "func_call" ? 2 : 3)..-1] = e
      call
    in :command, [_, name, _], [_, args, *_]
      ["func_call", name, *(args.map {|e_| simplify(e_) })]
    in (:if | :elsif), cond_exp, then_exp, *rest
      if rest[0]
        if rest[0][0] == :elsif
          else_exp = simplify(rest[0])
        else
          else_exp = make_stmts(rest[0][1])
        end
      end
      ["if", simplify(cond_exp), make_stmts(then_exp), else_exp]
    in :ifop, cond_exp, then_exp, else_exp
      ["if", simplify(cond_exp), simplify(then_exp), simplify(else_exp)]
    in :if_mod, cond_exp, then_exp
      ["if", simplify(cond_exp), make_stmts([then_exp]), nil]
    in :while, cond_exp, body_exp
      ["while", simplify(cond_exp), make_stmts(body_exp)]
    in :while_mod, cond_exp, [_, [_, body_exp, *_]]
      ["while2", simplify(cond_exp), make_stmts(body_exp)]
    in :binary, exp1, op, exp2
      [op.to_s, simplify(exp1), simplify(exp2)]
    in :var_ref, [:@kw, "nil", _]
      ["lit", nil]
    in :var_ref, [:@kw, "true", _]
      ["lit", true]
    in :var_ref, [:@kw, "false", _]
      ["lit", false]
    in :var_ref, [:@ident, name, _]
      ["var_ref", name]
    in :var_ref, [:@const, name, _]
      ["const_ref", name]
    in :@int, num, _
      ["lit", num.to_i]
    in :unary, _, value
      v = simplify(value)
      raise if v[0] != "lit"
      ["lit", -v[1]]
    in :string_literal, [_, value]
      ["lit", value ? value[1] : ""]
    in :symbol_literal, [_, [_, sym]]
      ["lit", sym.to_sym]
    in :assign, [:var_field, [_, name, _]], exp1
      ["var_assign", name, simplify(exp1)]
    in :assign, [:aref_field, ary, [_, [idx], *_]], exp1
      ["ary_assign", simplify(ary), simplify(idx), simplify(exp1)]
    in :case, arg, exp1, exp2
      when_clauses = []
      exp = exp1
      while exp && exp[0] == :when
        pat = arg.map {|e_| simplify(e_) }
        when_clauses << [pat, make_stmts(exp1)]
        exp = exp2
      end
      else_clause = make_stmts(arg) if exp
      #["case", arg, when_clauses, else_clause]

      exp = else_clause
      when_clauses.reverse_each do |patterns, stmts|
        patterns.each do |pattern|
          exp = ["if", ["==", simplify(arg), pattern], stmts, exp]
        end
      end
      exp
    in :method_add_block, method, [_, params, body]
      call = simplify(method)
      blk_params = params[1][1].map {|a| a[1] }
      blk_body = body.map {|e_| simplify(e_) }
      call << blk_params << blk_body
    in :aref, ary, [_, idxs, *_]
      ["ary_ref", simplify(ary), *idxs.map {|e_| simplify(e_) }]
    in :array, exp1
      ["ary_new", *(exp1 ? exp1.map {|e_| simplify(e_) } : [])]
    in [:hash]
      ["hash_new"]
    in :hash, [_, key_values]
      kvs = ["hash_new"]
      key_values.each do |e_|
        key = simplify(e_[1])
        val = simplify(e_[2])
        kvs << key << val
      end
      kvs
    in [:void_stmt]
      ["lit", nil]
    in :paren, exp1
      simplify(exp1[0])
    else
      pp exp
      raise "unsupported node: #{ exp[0] }"
    end
  end

  def make_stmts(exps)
    exps = exps.map {|exp| simplify(exp) }
    exps.size == 1 ? exps[0] : ["stmts", *exps]
  end
end

def minruby_load()
  File.read(ARGV.shift)
end

def minruby_parse(src)
  MinRubyParser.minruby_parse(src)
end

def minruby_call(mhd, args)
  send(mhd, *args)
end
