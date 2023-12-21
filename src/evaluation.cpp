#include "Def.hpp"
#include "value.hpp"
#include "expr.hpp"
#include "RE.hpp"
#include "syntax.hpp"
#include <cstring>
#include <vector>
#include <map>

extern std ::map<std ::string, ExprType> primitives;
extern std ::map<std ::string, ExprType> reserved_words;

Value ExprBase::eval(Assoc &env) { throw RuntimeError("RE"); }

Value Let::eval(Assoc &env) {} // let expression

Value Lambda::eval(Assoc &env) {} // lambda expression

Value Apply::eval(Assoc &e) {} // for function calling

Value Letrec::eval(Assoc &env) {} // letrec expression

Value Var::eval(Assoc &e) {} // evaluation of variable

Value Fixnum::eval(Assoc &e)
{
    ValueBase *pt = new Integer(n);
    Value temp(pt);
    return temp;
} // evaluation of a fixnum

Value If::eval(Assoc &e) {} // if expression

Value True::eval(Assoc &e)
{
    ValueBase *pt = new Boolean(true);
    Value temp(pt);
    return temp;
} // evaluation of #t

Value False::eval(Assoc &e)
{
    ValueBase *pt = new Boolean(false);
    Value temp(pt);
    return temp;
} // evaluation of #f

Value Begin::eval(Assoc &e) {} // begin expression

Value Quote::eval(Assoc &e)
{
    if (typeid(*s.get()) == typeid(List))
    {
        List *list = dynamic_cast<List *>(s.get());
        if (list->stxs.size() == 0)
            return NullV();

        // 处理第一个元素
        Syntax now = list->stxs[0];
        Quote fstq(now);
        Value v1 = fstq.eval(e);

        // 递归处理剩下元素
        List *nxt = new List();
        auto it = list->stxs.begin() + 1;
        nxt->stxs.assign(it, list->stxs.end());
        Syntax tt(nxt);
        Expr ttt(new Quote(tt));
        return Value(new Pair(v1, ttt->eval(e)));
    }
    else if (typeid(*s.get()) == typeid(Identifier))
    {
        Identifier *temp = dynamic_cast<Identifier *>(s.get());
        ValueBase *t = new Symbol(temp->s);
        return Value(t);
    }
    else
    {
        return s->parse(e)->eval(e);
    }
} // quote expression

Value MakeVoid::eval(Assoc &e) {} // (void)

Value Exit::eval(Assoc &e)
{
    ValueBase *temp = new Terminate();
    return Value(temp);
} // (exit)

Value Binary::eval(Assoc &e)
{
    Value v1 = rand1->eval(e);
    Value v2 = rand2->eval(e);
    return evalRator(v1, v2);
} // evaluation of two-operators primitive

Value Unary::eval(Assoc &e)
{
    Value v1 = rand->eval(e);
    return evalRator(v1);
} // evaluation of single-operator primitive

Value Mult::evalRator(const Value &rand1, const Value &rand2) {} // *

Value Plus::evalRator(const Value &rand1, const Value &rand2) {} // +

Value Minus::evalRator(const Value &rand1, const Value &rand2) {} // -

Value Less::evalRator(const Value &rand1, const Value &rand2) {} // <

Value LessEq::evalRator(const Value &rand1, const Value &rand2) {} // <=

Value Equal::evalRator(const Value &rand1, const Value &rand2) {} // =

Value GreaterEq::evalRator(const Value &rand1, const Value &rand2) {} // >=

Value Greater::evalRator(const Value &rand1, const Value &rand2) {} // >

Value IsEq::evalRator(const Value &rand1, const Value &rand2) {} // eq?

Value Cons::evalRator(const Value &rand1, const Value &rand2)
{
    ValueBase *temp = new Pair(rand1, rand2);
    return Value(temp);
} // cons

Value IsBoolean::evalRator(const Value &rand)
{
    return BooleanV(rand->v_type == V_BOOL);
} // boolean?

Value IsFixnum::evalRator(const Value &rand)
{
    return BooleanV(rand->v_type == V_INT);
} // fixnum?

Value IsSymbol::evalRator(const Value &rand)
{
    return BooleanV(rand->v_type == V_SYM);
} // symbol?

Value IsNull::evalRator(const Value &rand)
{
    return BooleanV(rand->v_type == V_NULL);
} // null?

Value IsPair::evalRator(const Value &rand)
{
    return BooleanV(rand->v_type == V_PAIR);
} // pair?

Value IsProcedure::evalRator(const Value &rand)
{
    return BooleanV(rand->v_type == V_PROC);
} // procedure?

Value Not::evalRator(const Value &rand)
{
    if (rand->v_type == V_BOOL)
    {
        Boolean *temp = dynamic_cast<Boolean *>(rand.get());
        if (temp->b == false)
            return BooleanV(1);
    }
    return BooleanV(0);
} // not

Value Car::evalRator(const Value &rand)
{
    if (rand->v_type != V_PAIR)
        throw RuntimeError("Bad value type (should be pair)");
    Pair *temp = dynamic_cast<Pair *>(rand.get());
    return temp->car;
} // car

Value Cdr::evalRator(const Value &rand)
{
    if (rand->v_type != V_PAIR)
        throw RuntimeError("Bad value type (should be pair)");
    Pair *temp = dynamic_cast<Pair *>(rand.get());
    return temp->cdr;
} // cdr
