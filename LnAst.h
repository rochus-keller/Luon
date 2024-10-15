#ifndef LUONAST_H
#define LUONAST_H

/*
** Copyright (C) 2024 Rochus Keller (me@rochus-keller.ch)
**
** This file is part of the Luon language project.
**
**
** GNU Lesser General Public License Usage
** This file may be used under the terms of the GNU Lesser
** General Public License version 2.1 or version 3 as published by the Free
** Software Foundation and appearing in the file LICENSE.LGPLv21 and
** LICENSE.LGPLv3 included in the packaging of this file. Please review the
** following information to ensure the GNU Lesser General Public License
** requirements will be met: https://www.gnu.org/licenses/lgpl.html and
** http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
*/

// adopted from Micron

#include <QByteArray>
#include <Luon/LnRowCol.h>
#include <QVariant>

namespace Ln
{
    class Declaration;

    struct BasicType
    {
        enum Type {
               Undefined,
               NoType,
               StrLit,
               ByteArrayLit,
               Any,
               Nil,
               BOOLEAN,
               CHAR,
               UINT32, UINT64,
               INT32, INT64,
               LONGREAL,
               SET,
               STRING,
               Max
             };

        static QVariant getMax(Type);
        static QVariant getMin(Type);
    };

    struct Builtin
    {
        enum Type {
            // functions
            ABS, CAP, BITAND, BITASR, BITNOT, BITOR, BITS, BITSHL, BITSHR,
            BITXOR, CAST, CHR, DEFAULT, EQUALS, FLOOR, FLT, GETENV, LEN, LONG, MAX,
            MIN, ODD, ORD, SHORT, SIGNED, SIZE, STRLEN, UNSIGNED, VARARG, VARARGS,
            // procedures
            ASSERT, COPY, DEC, DISPOSE, EXCL, HALT, INC,
            INCL, NEW, PCALL, PRINT, PRINTLN, RAISE, SETENV,
            // end
            Max
        };
    };

    struct Expression;

    class Type
    {
    public:
        enum Form { Record = BasicType::Max, Array, HashMap, Proc, ConstEnum, NameRef, Generic };
        quint8 form;
        bool deferred;
        bool anonymous;
        bool selfref;
        quint32 len; // array length
        Type* base; // array/pointer base type, return type
        QList<Declaration*> subs; // list of record fields or enum elements, or params for proc type
        Declaration* decl;
        Expression* expr; // array len, quali, key

        bool isUInt() const { return form >= BasicType::UINT32 && form <= BasicType::UINT64; }
        bool isInt() const { return form >= BasicType::INT32 && form <= BasicType::INT64; }
        bool isNumber() const { return form >= BasicType::UINT32 && form <= BasicType::LONGREAL; }
        bool isReal() const { return form == BasicType::LONGREAL; }
        bool isInteger() const { return form >= BasicType::UINT32 && form <= BasicType::INT64; }
        bool isSet() const { return form == BasicType::SET; }
        bool isBoolean() const { return form == BasicType::BOOLEAN; }
        bool isSimple() const { return form >= BasicType::StrLit && form < BasicType::Max; }
        bool isText() const { return form == BasicType::StrLit || form == BasicType::CHAR ||
                    ( form == Array && base && base->form == BasicType::CHAR ) ||
                    ( form == BasicType::STRING ); }
        bool isStructured() const { return form == Array || form == Record; }

        Declaration* findField(const QByteArray& name) const;
        QPair<int,int> getFieldCount() const; // fixed, variant

        Type():form(0),len(0),base(0),decl(0),deferred(false),anonymous(false),selfref(false),expr(0){}
        ~Type();
    };

    struct Statement;

    class Declaration
    {
    public:
        enum Mode { NoMode, Scope, Module, TypeDecl, Builtin, ConstDecl, Import, Field, Variant,
                    VarDecl, LocalDecl,
                    Procedure, ForwardDecl, ParamDecl,
                    Max };
        static const char* s_mode[];
        Declaration* next; // list of all declarations in outer scope
        Declaration* link; // member list or alias proc or imported module decl
        Declaration* outer; // the owning declaration to reconstruct the qualident
        Statement* body; // procs
        Type* type;
        QByteArray name;
        RowCol pos;
        enum Visi { NA, Private, ReadOnly, ReadWrite };
        uint visi : 2;
        enum Access { ByValue, IN, VAR };
        uint access : 2;
        uint inline_ : 1;
        uint invar : 1;
        uint meta : 1;
        uint scope : 1;
        uint ownstype : 1;
        uint mode : 5;
        uint id : 16; // used for built-in code and local/param number
        QVariant data; // value for Const and Enum, path for Import, name for Extern
        Expression* expr; // const decl, enum, meta actuals
        Declaration():next(0),link(0),type(0),body(0),id(0),mode(0),visi(0),ownstype(false),expr(0),
            inline_(false),invar(false),meta(false),scope(false),outer(0),access(0){}
        ~Declaration();

        QList<Declaration*> getParams() const;
        int getIndexOf(Declaration*) const;
        bool isLvalue() const { return mode == VarDecl || mode == LocalDecl || mode == ParamDecl; }
        bool isPublic() const { return visi >= ReadOnly; }
    };
    typedef QList<Declaration*> DeclList;

    class Expression {
    public:
        enum Kind {
            Invalid,
            Plus, Minus, Not, // Unary
            Eq, Neq, Lt, Leq, Gt, Geq, In, // Relation
            Add, Sub, Or, // AddOp
            Mul, Fdiv, Div, Mod, And, // MulOp
            LocalVar, Param, Builtin, // val is index of local or param or builtin
            ModuleVar, ProcDecl, ConstDecl, TypeDecl, // val is declaration
            Select, // f.g, val is field declaration
            Index, // a[i]
            Cast, AutoCast,
            Call,
            Literal, Set, Range,
            NameRef, // temporary, will be resolved by validator
            MAX
        };
#ifdef _DEBUG
        Kind kind;
#else
        quint8 kind;
#endif
        bool byVal; // option for LocalVar, Param, ModuleVar, Select, Index
        bool needsLval;
        quint8 visi;
        RowCol pos;
        Type* type;
        QVariant val; // set elements and call args are ExpList embedded in val
        Expression* lhs; // for unary and binary ops
        Expression* rhs; // for binary ops
        Expression* next; // for args, set elems, and caselabellist
        bool isConst() const;
        bool isLiteral() const;
        QVariant getLiteralValue() const;
        DeclList getFormals() const;
        bool isLvalue() const; // true if result of expression is usually a ref to type; can be changed with byVal
        void setByVal();
        static int getNumOfArgs(const Expression*);
        static void appendArg(Expression* exp, Expression* arg);
        static Expression* createFromToken(quint16,const RowCol&);

        Expression(Kind k = Invalid, const RowCol& rc = RowCol()):kind(k),type(0),lhs(0),rhs(0),next(0),needsLval(false),
            pos(rc),byVal(false),visi(0){}
        ~Expression();
    };

    struct Value {
        enum Mode { None, Val, Const, Builtin, Procedure, VarDecl, LocalDecl, ParamDecl, TypeDecl };
        quint8 mode;
        quint8 visi;
        bool ref; // the value is a reference to the type
        Type* type;
        QVariant val;

        Value():mode(0),type(0),ref(false),visi(Declaration::Private){}
        Value(Type* t, const QVariant& v, Mode m):type(t),val(v),mode(m){}

        bool isConst() const { return mode == Const; }
        bool isLvalue() const { return mode == Declaration::VarDecl || mode == Declaration::LocalDecl ||
                    mode == Declaration::ParamDecl; }
        bool isCallable() const;
    };
    typedef QList<Value> MetaActualList;

    struct Import {
        QByteArrayList path;
        MetaActualList metaActuals;
    };

    struct Statement {
        enum Kind { Invalid,
            Assig, Call, If, Elsif, Else, Case, CaseLabel,
            Loop, While, Repeat, Exit, Return, ForAssig, ForToBy
        };
        quint8 kind;

        RowCol pos;
        Expression* lhs; // proc, assig lhs
        Expression* rhs; // rhs, args, cond, case, label, return
        Statement* next; // list of statements
        Statement* body; // then
        Statement* getLast() const;
        Statement(Kind k = Invalid, const RowCol& pos = RowCol()):kind(k),pos(pos),lhs(0),rhs(0),next(0),body(0) {}
        ~Statement();
    };

    typedef QList<Declaration*> MetaParamList;

    struct ModuleData {
        QByteArrayList path;
        MetaParamList metaParams;
        MetaActualList metaActuals;
        QByteArray suffix;
        QByteArray fullName; // path.join('/') + suffix as symbol
    };

    class AstModel
    {
    public:
        AstModel();
        ~AstModel();

        void openScope(Declaration* scope);
        Declaration* closeScope(bool takeMembers = false);
        Declaration* addDecl(const QByteArray&, bool* doublette = 0 );
        Declaration* addHelper();
        void removeDecl(Declaration*);
        Declaration* findDecl(const QByteArray&, bool recursive = true) const;
        Declaration* findDecl(Declaration* import, const QByteArray&) const;
        Declaration* getTopScope() const { return scopes.back(); }
        QByteArray getTempName();
        Declaration* getTopModule() const;

        Type* getType(quint8 basicType) const { return types[basicType]; }

        static void cleanupGlobals();
    protected:
        Type* newType(int form, int size);
        Type* addType(const QByteArray& name, int form, int size);
        void addTypeAlias(const QByteArray& name, Type*);
        void addBuiltin(const QByteArray& name, Builtin::Type);
    private:
        QList<Declaration*> scopes;
        Declaration* helper;
        quint32 helperId;
        static Declaration globalScope;
        static Type* types[BasicType::Max];

    };
}

Q_DECLARE_METATYPE(Ln::Import)
Q_DECLARE_METATYPE(Ln::Declaration*)
Q_DECLARE_METATYPE(Ln::Expression*)
Q_DECLARE_METATYPE(Ln::ModuleData)


#endif // MICAST_H
