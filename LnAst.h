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
        enum { SignedIntBitWidth = 52 };
        enum Type {
               Undefined,
               NoType,
               StrLit,
               ByteArrayLit,
               Nil,
               BOOLEAN,
               CHAR,
               BYTE,
               INTEGER, // max. representable signed int in LuaJIT, use MAX and MIN
               REAL,    // double
               SET,     // cut at uint32
               STRING,
               ANYREC,
               Max
             };

        static QVariant getMax(quint8 form);
        static QVariant getMin(quint8 form);
        static const char* name[];
    };

    struct Builtin
    {
        enum Type {
            // functions
            ABS, CAP, BITAND, BITASR, BITNOT, BITOR, BITS, BITSHL, BITSHR,
            BITXOR, CAST, CHR, CLIP, DEFAULT, FLOOR, FLT, GETENV, LEN, MAX,
            MIN, ODD, ORD, STRLEN,
            // procedures
            ASSERT, COPY, DEC, EXCL, HALT, INC,
            INCL, NEW, PCALL, PRINT, PRINTLN, RAISE, SETENV,
            // end
            TRAP, TRAPIF,
            Max
            // NOTE: CAST is only used to convert integers to enums
        };
        static const char* name[];
    };

    class Expression;

    class Type
    {
    public:
        enum Form { Record = BasicType::Max, Array, HashMap, Proc, ConstEnum, NameRef, Generic };
        uint form : 8;
        uint deferred : 1;
        uint anonymous : 1;
        uint validated : 1;
        uint allocated : 1;
        uint receiver : 1;
        quint32 len; // array length
        Type* base; // array/pointer base type, return type
        QList<Declaration*> subs; // list of record fields or enum elements, or params for proc type
        Declaration* decl; // if NameRef includes pos and name
        Expression* expr; // array len, hashmap key

        bool isNumber() const { return form == BasicType::INTEGER || form == BasicType::REAL || form == BasicType::BYTE; }
        bool isReal() const { return form == BasicType::REAL; }
        bool isInteger() const { return form == BasicType::INTEGER || form == BasicType::BYTE;  }
        bool isSet() const { return form == BasicType::SET; }
        bool isBoolean() const { return form == BasicType::BOOLEAN; }
        bool isSimple() const { return form >= BasicType::StrLit && form < BasicType::Max; }
        bool isReference() const {return form >= Record && form <= Proc; }
        bool isStructured() const { return form == Array || form == Record || form == HashMap; }
        QPair<int,int> countAllocRecordMembers(bool recursive = false);
        static bool isSubtype(Type* super, Type* sub);

        bool isDerefCharArray() const;
        bool isDerefByteArray() const;
        Type* deref() const;

        Declaration* find(const QByteArray& name, bool recursive = true) const;
        QList<Declaration*> fieldList() const;
        QList<Declaration*> methodList(bool recursive = false) const;

        Type():form(0),len(0),base(0),decl(0),deferred(false),anonymous(false),
            expr(0),validated(false),allocated(false),receiver(false){}
        ~Type();
    };

    class Statement;

    class Declaration
    {
    public:
        enum Kind { Invalid, Helper, Scope, Module, TypeDecl, Builtin, ConstDecl, Import, Field,
                    VarDecl, LocalDecl,
                    Procedure, ParamDecl,
                    Max };
        Declaration* link; // member list or imported module decl
        Declaration* outer; // the owning declaration to reconstruct the qualident
        Declaration* super; // super class or overridden method
        Statement* body; // procs
        Type* type;
        QByteArray name;
        RowCol pos;
        enum Visi { NA, Private, ReadOnly, ReadWrite };
        uint visi : 2;
        uint varParam : 1; // var param
        enum Mode { Normal, Begin, Receiver, Inline, Invar, Extern, Meta, Enum };
        uint mode : 3;
        uint ownstype : 1;
        uint inList : 1; // private
        uint validated : 1;
        uint hasErrors : 1;
        uint hasSubs : 1; // class/method: has overrides; module: has clients
        uint kind : 4;
        uint id : 16; // used for built-in code and local/param number
        QVariant data; // value for Const and Enum, path for Import, name for Extern
        Expression* expr; // const decl, enum, meta actuals

        Declaration():next(0),link(0),type(0),body(0),id(0),kind(0),mode(0), visi(0),ownstype(false),expr(0),
            outer(0),varParam(0),inList(0),validated(0), super(0), hasSubs(0), hasErrors(0){}

        QList<Declaration*> getParams(bool skipReceiver = false) const;
        int getIndexOf(Declaration*) const;
        bool isLvalue() const { return kind == VarDecl || kind == LocalDecl || kind == ParamDecl; }
        bool isPublic() const { return visi >= ReadOnly; }
        Declaration* getNext() const { return next; }
        Declaration* getLast() const;
        Declaration* find(const QByteArray& name, bool recursive = true);
        Declaration* getModule();
        void appendMember(Declaration*);
        RowCol getEndPos() const;
        QByteArray scopedName(bool withModule = false, bool withPath = false) const;
        static void deleteAll(Declaration*);

    private:
        ~Declaration();
        Declaration* next; // list of all declarations in outer scope
        friend class AstModel;
    };
    typedef QList<Declaration*> DeclList;

    class Expression {
    public:
        enum Kind {
            Invalid,
            Plus, Minus, Not, // Unary
            Eq, Neq, Lt, Leq, Gt, Geq, In, Is, // Relation
            Add, Sub, Or, // AddOp
            Mul, Fdiv, Div, Mod, And, // MulOp
            DeclRef, // val is declaration
            Select, // f.g, val is field declaration
            Index, // a[i]
            Cast,
            Call,
            Literal,
            Constructor, Range, KeyValue,
            NameRef, // temporary, will be resolved by validator to DeclRef and ConstVal
            ConstVal,
            Super,   // ^ supercall
            MAX
        };
#ifdef _DEBUG
        Kind kind;
#else
        quint8 kind;
#endif
        uint byVal : 1; // option for LocalVar, Param, ModuleVar, Select, Index
        uint needsLval : 1;
        uint visi : 2;
        uint byName : 1; // true if lhs of KeyValue is not an index or key, but a name
        RowCol pos;
        Type* type;
        QVariant val; // set elements and call args are ExpList embedded in val
        Expression* lhs; // for unary and binary ops
        Expression* rhs; // for binary ops
        Expression* next; // for args, set elems, and caselabellist

        bool isConst() const;
        DeclList getFormals() const;
        bool isLvalue() const; // true if result of expression is usually a ref to type; can be changed with byVal
        void setByVal();
        bool isCharLiteral();
        qint64 getCaseValue(bool* ok = 0) const;
        void appendRhs(Expression*);
        static int getCount(const Expression* list);
        static void append(Expression* list, Expression* elem);
        static QList<Expression*> getList(Expression* exp);
        static Expression* createFromToken(quint16,const RowCol&);

        Expression(Kind k = Invalid, const RowCol& rc = RowCol()):
            kind(k),type(0),lhs(0),rhs(0),next(0),needsLval(false),
            pos(rc),byVal(false),visi(0),byName(0){}
        ~Expression();
    };

    typedef QList<Expression*> ExpList;

    typedef QList<Expression*> MetaActualList;

    struct Import {
        QByteArrayList path;    // full path incl. name
        MetaActualList metaActuals; // ref to importDecl->expr, deleted there
        Declaration* importer;
        RowCol importedAt;
        Declaration* resolved; // module
        Import():resolved(0),importer(0){}
    };

    class Statement {
    public:
        enum Kind { Invalid,
            Assig, Call, If, Elsif, Else, Case, TypeCase, CaseLabel,
            Loop, While, Repeat, Exit, Return, ForAssig, ForToBy, End
        };
        quint8 kind;

        RowCol pos;
        Expression* lhs; // owns: proc, assig lhs
        Expression* rhs; // owns: rhs, args, cond, case, label, return
        Statement* body; // owns: then

        Statement(Kind k = Invalid, const RowCol& pos = RowCol()):kind(k),pos(pos),lhs(0),rhs(0),
            next(0),body(0),inList(false) {}
        Statement* getLast() const;
        Statement* getNext() const { return next; }
        void append(Statement*s);
        static void deleteAll(Statement*);
    private:
        ~Statement();

        Statement* next; // owns: list of statements
        bool inList;
    };

    typedef QList<Declaration*> MetaParamList;

    struct ModuleData {
        QByteArrayList path;
        QString source;
        MetaParamList metaParams;
        MetaActualList metaActuals;
        QByteArray suffix;
        QByteArray fullName; // path.join('/') + suffix as symbol
        RowCol end;
    };

    typedef QPair<QByteArray,QByteArray> Qualident;

    class Symbol
    {
    public:
        enum Kind { Invalid, Module, Decl, DeclRef, Lval };
        quint8 kind;
        quint8 len;
        RowCol pos;
        Declaration* decl;
        Symbol* next;
        Symbol():kind(Invalid),len(0),decl(0),next(0){}
        static void deleteAll(Symbol*);
    };

    typedef QList<Symbol*> SymList;

    struct Xref {
        Symbol* syms;
        QHash<Declaration*,SymList> uses;
        QHash<Declaration*,DeclList> subs;
        Xref():syms(0){}
    };


    class AstModel
    {
    public:
        AstModel();
        ~AstModel();

        void openScope(Declaration* scope);
        Declaration* closeScope(bool takeMembers = false);
        Declaration* addDecl(const QByteArray&);
        Declaration* addHelper();
        Declaration* findDecl(const QByteArray&, bool recursive = true) const;
        Declaration* findDecl(Declaration* import, const QByteArray&) const;
        Declaration* getTopScope() const;
        QByteArray getTempName();
        Declaration* getTopModule() const;

        Type* getType(quint8 basicType) const { return types[basicType]; }

        static void cleanupGlobals();
        static DeclList toList(Declaration* d);
    protected:
        Type* newType(int form, int size);
        Type* addType(const QByteArray& name, int form, int size);
        void addTypeAlias(const QByteArray& name, Type*);
        void addBuiltin(const QByteArray& name, Builtin::Type);
    private:
        QList<Declaration*> scopes;
        Declaration* helper;
        quint32 helperId;
        static Declaration* globalScope;
        static Type* types[BasicType::Max];

    };
}

Q_DECLARE_METATYPE(Ln::Import)
Q_DECLARE_METATYPE(Ln::Declaration*)
Q_DECLARE_METATYPE(Ln::Expression*)
Q_DECLARE_METATYPE(Ln::Symbol*)
Q_DECLARE_METATYPE(Ln::ModuleData)


#endif // MICAST_H
