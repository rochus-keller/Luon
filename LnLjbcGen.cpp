/*
* Copyright 2024 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Luon parser/compiler library.
*
* The following is the license that applies to this copy of the
* library. For a license to use the library under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* GNU General Public License Usage
* This file may be used under the terms of the GNU General Public
* License (GPL) versions 2.0 or 3.0 as published by the Free Software
* Foundation and appearing in the file LICENSE.GPL included in
* the packaging of this file. Please review the following information
* to ensure GNU General Public Licensing requirements will be met:
* http://www.fsf.org/licensing/licenses/info/GPLv2.html and
* http://www.gnu.org/copyleft/gpl.html.
*/

#include "LnLjbcGen.h"
#include <LjTools/LuaJitComposer.h>
#include <QtDebug>
using namespace Ln;

class LjbcGen::Imp
{
public:
    struct NoMoreFreeSlots {};
    enum { MAX_PROC_SLOTS = 230 };

    struct Ctx
    {
        Declaration* scope;
        Lua::JitComposer::SlotPool pool;
        // module is the top level proc and each other proc is a sub-proc of module; there are no sub-sub-procs
        typedef QHash<quint8,QPair<quint16,QByteArray> > Upvals;
        // slot -> upval; only for sub-procs; slot is in module and read-only
        Upvals upvals;
        Ctx(Declaration* s = 0):scope(s) { }

        int usedCount() const { return pool.d_slots.count(); }

        int buySlots(int len = 1, bool call = false )
        {
            const int tmp = Lua::JitComposer::nextFreeSlot(pool,len, call );
            if( tmp < 0 )
                throw NoMoreFreeSlots();
            return tmp;
        }
        void sellSlots(quint8 base, int len = 1 )
        {
            Lua::JitComposer::releaseSlot(pool,base, len );
        }
        quint16 resolveUpval(quint8 slot, const QByteArray& name)
        {
            Upvals::const_iterator i = upvals.find(slot);
            if( i != upvals.end() )
                return i.value().first;
            const int nr = upvals.size();
            QPair<quint16,QByteArray>& uv = upvals[ slot ];
            uv.first = nr;
            uv.second = name;
            return nr;
        }
    };

    QList<LjbcGen::Error> errors;
    Lua::JitComposer bc;
    QList<Ctx> ctx;
    quint8 modSlot, lnlj;
    Declaration* thisMod;
    AstModel mdl;
    QMap<Declaration*, quint32> imports; // module -> slot
    QList<Declaration*> deferred;
    QList<quint8> slotStack; // for expression evaluation
    QList <quint32> exitJumps; // TODO: must be a separate list for each (nested) LOOP
    bool hasExterns, doDeferImports;

    struct Lvalue
    {
        enum Kind { Invalid,
                    TableIdxSlot, // for Index
                    TableIdx, // for Select
                    Slot };
        uint kind : 3;
        uint disposeSlot : 1;
        uint disposeIndex : 1;
        uint slot : 8;
        uint index : 16;
        Lvalue():kind(Invalid),disposeSlot(0),disposeIndex(0),slot(0),index(0){}
    };

    Imp():modSlot(0),lnlj(0),thisMod(0),hasExterns(false),doDeferImports(false) {}

    void createAllClassObjects(Declaration* scope)
    {
        // each record type is represented by a class object at runtime which is essentially
        // the virtual table accomodating bound procedures.
        // the class object is referenced by each instance of a record via the meta table slot of
        // a lua table. also the super class is referenced via meta table slot of the class object.

        Declaration* d = scope->link;
        while( d )
        {
            switch( d->kind )
            {
            case Declaration::TypeDecl:
                if( d->type && d == d->type->decl && d->type->form == Type::Record )
                {
                    Type* record = d->type;
                    const QPair<int, int> counts = record->countAllocRecordMembers(true);
                    const int curClass = ctx.back().buySlots(1);
                    bc.TNEW( curClass, counts.second, 2, d->pos.packed() );

                    Q_ASSERT( ctx.back().scope == thisMod );

                    // store the record slot number and module reference in each class object
                    const int tmp = ctx.back().buySlots(1);
                    bc.KSET(tmp, d->id, d->pos.packed() );
                    emitSetTableByName(tmp, curClass, "@cls", d->pos );
                    emitSetTableByName(modSlot, curClass, "@mod", d->pos );
                    bc.KSET(tmp, d->scopedName(true, true), d->pos.packed());
                    emitSetTableByName(tmp, curClass, "@name", d->pos );
                    ctx.back().sellSlots(tmp);
                    ctx.back().sellSlots(curClass);

                    emitSetTableByIndex( curClass, modSlot, d->id, d->pos );

                    foreach( Declaration* p, record->subs )
                    {
                        if( p->kind == Declaration::Procedure )
                            createAllClassObjects(p);
                    }
                }
                break;
            case Declaration::Procedure:
                createAllClassObjects(d);
                break;
            }

            d = d->getNext();
        }
    }

    void connectAllClassObjects(Declaration* scope)
    {
        Declaration* d = scope->link;
        while( d )
        {
            switch( d->kind )
            {
            case Declaration::TypeDecl:
                if( d->type && d == d->type->decl && d->type->form == Type::Record )
                {
                    Type* record = d->type;


                    if( record->base )
                    {
                        Q_ASSERT( ctx.back().scope == thisMod );
                        const int curClass = ctx.back().buySlots(1);
                        emitGetTableByIndex( curClass, modSlot, d->id, d->pos );

                        const int baseClass = ctx.back().buySlots(1);
                        fetchClass(baseClass, record->base, d->pos);
#ifdef _DEBUG
                        emitAssureNotNil(curClass,QString("connectAllClassObjects metaclass of %1 is nil in %2")
                                         .arg(d->scopedName(true,true).constData())
                                         .arg(thisMod->name.constData()), d->pos);
#endif
                        const int tmp = ctx.back().buySlots(3,true);
                        fetchLnlibMember(tmp, 22, d->pos); // setmetatable
                        bc.MOV( tmp+1, curClass, d->pos.packed() );
                        bc.MOV(tmp+2,baseClass, d->pos.packed() );
                        bc.CALL( tmp, 0, 2, d->pos.packed() );
                        ctx.back().sellSlots(tmp,3);
                        ctx.back().sellSlots(baseClass);
                        ctx.back().sellSlots(curClass);
                    }

                    foreach( Declaration* p, record->subs )
                    {
                        if( p->kind == Declaration::Procedure )
                            connectAllClassObjects(p);
                    }
                }
                break;
            case Declaration::Procedure:
                connectAllClassObjects(d);
                break;
            }
            d = d->getNext();
        }
    }

    void emitAllImports(Declaration* scope)
    {
        Declaration* d = scope->link;
        while( d )
        {
            switch( d->kind )
            {
            case Declaration::Import:
                emitImport(d);
                break;
            }
            d = d->getNext();
        }
    }

    void emitAllProcedures(Declaration* scope)
    {
        Declaration* d = scope->link;
        while( d )
        {
            switch( d->kind )
            {
            case Declaration::TypeDecl:
                if( d->type && d == d->type->decl && d->type->form == Type::Record )
                {
                    foreach( Declaration* p, d->type->subs )
                        if( p->kind == Declaration::Procedure )
                            emitAllProcedures(p);

                    Q_ASSERT( ctx.back().scope == thisMod );
                    const int curClass = ctx.back().buySlots(1);
                    emitGetTableByIndex( curClass, modSlot, d->id, d->pos );
                    foreach( Declaration* p, d->type->subs )
                    {
                        if( p->kind == Declaration::Procedure )
                            emitProcedure(p,curClass);
                    }
                    ctx.back().sellSlots(curClass);
                }
                break;
            case Declaration::Procedure:
                emitAllProcedures(d);
                emitProcedure(d);
                break;
            }
            d = d->getNext();
        }
    }

    void downcopyRecord(Type* record, QSet<Type*>& seen)
    {
        if( seen.contains(record) )
            return;
        seen.insert(record);
        if( record->decl->getModule() != thisMod )
            return;

        if( record->base )
        {
            Type* base = deref(record->base);
            if( base->form == Type::Record )
                downcopyRecord(base,seen);

            Declaration* d = record->decl;
            Q_ASSERT( ctx.back().scope == thisMod );
            const int curClass = ctx.back().buySlots(1);
            emitGetTableByIndex( curClass, modSlot, d->id, d->pos );

            const int baseClass = ctx.back().buySlots(1);
            fetchClass(baseClass, record->base, d->pos);
            const int tmp = ctx.back().buySlots(1);
            const QPair<int, int> counts = base->countAllocRecordMembers(true);
            QList<Declaration*> methods = record->methodList();
            QSet<int> used;
            foreach( Declaration* d, methods )
                used.insert(d->id);

            for( int i = 0; i < counts.second; i++ )
            {
                if( used.contains(i) )
                    continue; // don't overwrite the methods bound to this specific class
                emitGetTableByIndex(tmp,baseClass,i,record->decl->pos);
                emitSetTableByIndex(tmp,curClass,i,record->decl->pos);
            }

            ctx.back().sellSlots(tmp);
            ctx.back().sellSlots(baseClass);
            ctx.back().sellSlots(curClass);
        }
    }

    void downcopyAllMethods(Declaration* scope, QSet<Type*>& seen)
    {
        Declaration* d = scope->link;
        while( d )
        {
            if( d->kind == Declaration::TypeDecl && d->type && d == d->type->decl && d->type->form == Type::Record )
            {
                downcopyRecord(d->type, seen);
                foreach( Declaration* p, d->type->subs )
                {
                    if( p->kind == Declaration::Procedure )
                        downcopyAllMethods(p, seen);
                }
            }
            d = d->getNext();
        }
    }

    void emitPrint(const QString& msg, const RowCol& pos)
    {
        const int tmp = ctx.back().buySlots(2, true);
        bc.GGET(tmp, "print", pos.packed());
        bc.KSET(tmp+1,msg,pos.packed());
        bc.CALL(tmp, 0, 1, pos.packed());
        ctx.back().sellSlots(tmp,2);
    }

    bool visitModule(Declaration* module, QIODevice* out, bool strip)
    {
        Q_ASSERT( module && module->kind == Declaration::Module );

        // This function constructs the module construction function. The module is
        // a lua table used as an array. All procedures have access to this table via
        // its upval slot in the construction function.
        // The module table accommodates all module data as array elements indexed by
        // the allocated id.

        thisMod = module;
        ModuleData md = module->data.value<ModuleData>();

        ctx.push_back( Imp::Ctx(module) );
        bc.openFunction(0,md.fullName,module->pos.packed(), md.end.packed() );

#ifdef _DEBUG
        // emitPrint(QString("start generating %1").arg(md.fullName.constData()),module->pos);
#endif

        QHash<quint8,QByteArray> names;
        modSlot = ctx.back().buySlots(1);
        names[modSlot] = "@mod";
        lnlj = ctx.back().buySlots(1);
        names[lnlj] = "@lnlj";

        bc.TNEW( modSlot, module->id, 0, module->pos.packed() );

        emitImport("LUON", lnlj, module->pos, true, true );

        // make Module table a global variable (at the start to allow generic instances to access it
        bc.GSET( modSlot, md.fullName, md.end.packed() );
        const int tmp = ctx.back().buySlots(1);
        // save the module path in the module table under the name "@mod"
        bc.KSET(tmp, md.fullName, md.end.packed() );
        emitSetTableByName(tmp,modSlot,"@mod", md.end );
        ctx.back().sellSlots(tmp);

        // first create all class objects, because imported generic instances need them
        createAllClassObjects(module);
        // then make sure all modules are initialized this one depends on
        emitAllImports(module);
        connectAllClassObjects(module);

        doDeferImports = true;
        // collect implicit imports in all procedures, but execute them in top procedure
        emitAllProcedures(module);
        doDeferImports = false;

        foreach( Declaration* m, deferred )
            emitDeferredImports(m,md.end);
        deferred.clear();

        // at this point, methods are not yet downcopied from their superclass
        QSet<Type*> seen;
        downcopyAllMethods(module, seen);

        Declaration* d = module->link;
        Declaration* begin = 0;
        while( d )
        {
            switch( d->kind )
            {
            case Declaration::ConstDecl:
                if( d->mode != Declaration::Meta )
                    emitConst(d);
                break;
            case Declaration::VarDecl: {
                    const int val = ctx.back().buySlots(1);
                    emitInitializer(val,d->type,d->pos);
                    emitSetTableByIndex(val,modSlot,d->id,d->pos);
                    ctx.back().sellSlots(val);
                    break;
                }
            case Declaration::Procedure:
                if( d->mode == Declaration::Begin)
                    begin = d;
                break;
            }
            d = d->getNext();
        }

        if( hasExterns )
        {
            const int tmp = ctx.back().buySlots(2,true);
            bc.GGET( tmp, "require", md.end.packed() ); // just load the module so globals are created
            bc.KSET( tmp+1, "_" + md.path.join('_'), md.end.packed() );
            bc.CALL( tmp, 1, 1, md.end.packed() );
            ctx.back().sellSlots(tmp, 2);
        }

        if( begin )
        {
#if 0
            // we cannot directly call begin here, i.e. from the function run by require, because
            // it might call a LDMOD which could depend on the currently loading module, which LuaJIT regards
            // as an error; we therefore use LUON.require which calls begin after require and only once
            const int tmp = ctx.back().buySlots(1,true);
            emitGetTableByIndex(tmp, modSlot, begin->id, begin->pos);
            bc.CALL(tmp,0,0, begin->pos.packed());
            ctx.back().sellSlots(tmp);
#endif
        }

#ifdef _DEBUG
        //emitPrint(QString("finished generating %1").arg(md.fullName.constData()),module->pos);
#endif

        bc.UCLO( 0, 0, md.end.packed() );
        bc.RET( modSlot, 1, md.end.packed() ); // return module

        Lua::JitComposer::VarNameList sn(ctx.back().pool.d_frameSize);
        QHash<quint8,QByteArray>::const_iterator vi;
        for( vi = names.begin(); vi != names.end(); ++vi )
        {
            Lua::JitComposer::VarName& n = sn[vi.key()];
            n.d_name = vi.value();
            n.d_to = bc.getCurPc();
        }
        bc.setVarNames( sn );

        bc.closeFunction(ctx.back().pool.d_frameSize); // module function
        ctx.pop_back();

        if( errors.isEmpty() )
            bc.write(out);
        return errors.isEmpty();
    }

    void emitConst(Declaration* d)
    {
        const int val = ctx.back().buySlots(1);
        const int tbl = ctx.back().buySlots(1);

        fetchModule(tbl,d->pos);
        emitConstValue(val, d->data, d->type, d->pos);
        emitSetTableByIndex(val,tbl,d->id,d->pos);
        ctx.back().sellSlots(val);
        ctx.back().sellSlots(tbl);
    }

    void emitConstValue(quint8 to, const QVariant& data, Type* type, const RowCol& pos)
    {
        Declaration* d = data.value<Declaration*>();
        if( d )
        {
            switch( d->kind )
            {
            case Declaration::Procedure:
                // this happens when a procedure is passed as a meta actual
                fetchModule(d->getModule(),to, pos);
                emitGetTableByIndex(to, to, d->id, pos);
                break;
            case Declaration::ConstDecl:
                emitConstValue(to, d->data, d->type, pos);
                break;
            default:
                Q_ASSERT(false);
            }
            return;
        }
        Type* t = deref(type);
        switch( t->form )
        {
        case BasicType::StrLit:
        case BasicType::ByteArrayLit:
        case BasicType::STRING:
        case BasicType::BOOLEAN:
        case BasicType::CHAR:
        case BasicType::BYTE:
        case BasicType::INTEGER:
        case BasicType::REAL:
        case BasicType::SET:
        case Type::ConstEnum:
            bc.KSET(to, data, pos.packed() );
            break;
        case BasicType::Nil:
            bc.KNIL(to,1,pos.packed());
            break;
        default:
            Q_ASSERT(false);
        }
    }

    void emitProcedure(Declaration* p, int curClass = -1)
    {
        const RowCol end = p->getEndPos();
        DeclList params = p->getParams();
        ctx.push_back( Ctx(p) );
        const int id = bc.openFunction( params.size(), p->scopedName(true,true), p->pos.packed(), end.packed() );
        Q_ASSERT( id >= 0 );

        QHash<quint8,QByteArray> names;

        Declaration* d = p->link;
        int pars = 0, vpars = 0;
        while( d )
        {
            switch( d->kind )
            {
            case Declaration::TypeDecl:
            case Declaration::Procedure:
                // done on module level for all nested record decls and procedures
                break;
            case Declaration::ConstDecl:
            case Declaration::Import:
                // we don't need this
                break;
            case Declaration::LocalDecl:
            case Declaration::ParamDecl: {
                    const quint8 slot = ctx.back().buySlots(1);
                    Q_ASSERT( slot == d->id );
                    if( d->kind == Declaration::LocalDecl )
                        emitInitializer(slot,d->type,d->pos);
                    else
                    {
                        if( d->varParam )
                            vpars++;
                        pars++;
                    }
                    names[d->id] = d->name;
                    break;
                }
            }

            d = d->getNext();
        }

        if( p->mode == Declaration::Extern )
        {
            hasExterns = true;
            const int tmp = ctx.back().buySlots(pars+1,true);
            ModuleData md = thisMod->data.value<ModuleData>();
            bc.GGET(tmp, md.path.join('_') + "_" + p->name, p->pos.packed());
            // we expec the EXTERN proc to be implemented under the name "<module>_<proc>", e.g Out_String
            // whereas '_' is also used as the module path separator (to be compatible with Lua source code)
            for( int i = 0; i < pars; i++ )
                bc.MOV(tmp+i+1, i, p->pos.packed());
            bc.CALL(tmp, 1+vpars,pars,p->pos.packed());
            bc.RET(tmp, 1+vpars, p->pos.packed());
            ctx.back().sellSlots(tmp, pars+1);
        }else
        {
            Statement* stat = p->body;
            Statement* prev = 0;
            while( stat && stat->getNext() )
            {
                emitStatement(stat);
                prev = stat;
                stat = stat->getNext();
            }
            Q_ASSERT(stat == 0 || stat->kind == Statement::End);

            if( prev == 0 || prev->kind != Statement::Return )
                emitReturn( 0, end );
            // we need the full emitReturn here instead of only bc.RET(), because there
            // are proper procs with var params
        }

        Lua::JitComposer::VarNameList sn(ctx.back().pool.d_frameSize);
        QHash<quint8,QByteArray>::const_iterator vi;
        for( vi = names.begin(); vi != names.end(); ++vi )
        {
            Lua::JitComposer::VarName& n = sn[vi.key()];
            n.d_name = vi.value();
            n.d_to = bc.getCurPc();
        }
        bc.setVarNames( sn );
        Lua::JitComposer::UpvalList uv(ctx.back().upvals.size());
        Ctx::Upvals::const_iterator uvi;
        for( uvi = ctx.back().upvals.begin(); uvi != ctx.back().upvals.end(); ++uvi )
        {
            Lua::JitComposer::Upval u;
            u.d_name = uvi.value().second;
            u.d_isRo = true;
            u.d_isLocal = true;
            u.d_uv = uvi.key();
            uv[uvi.value().first] = u;
        }
        bc.setUpvals(uv);
        bc.closeFunction(ctx.back().pool.d_frameSize);
        ctx.pop_back();

        const int tmp = ctx.back().buySlots(1,true);
        bc.FNEW( tmp, id, end.packed() );

        if( p->mode == Declaration::Receiver )
        {
            Q_ASSERT(curClass >= 0);
            emitSetTableByIndex( tmp, curClass, p->id, end );
        }else
        {
            Q_ASSERT(ctx.back().scope == thisMod);
            emitSetTableByIndex( tmp, modSlot, p->id, end );
            if( ( p->type == 0 || p->type->form == BasicType::NoType) && params.size() == 0 )
            {
                // also store command procedures by name in the module table
                // this includes $begin if present
                emitSetTableByName(tmp, modSlot, p->name, end );
            }
        }
        ctx.back().sellSlots(tmp);
    }

    void jumpTrueFalse( quint8 res, const RowCol& loc )
    {
        // if true
        emitJMP(0,loc.packed());
        const quint32 pc1 = bc.getCurPc();
        bc.KSET(res, false, loc.packed() );
        emitJMP(0,loc.packed());
        const quint32 pc2 = bc.getCurPc();
        bc.patch(pc1);
        bc.KSET(res, true, loc.packed() );
        bc.patch(pc2);
    }

    void pushCharLiteral(Expression* e)
    {
        Q_ASSERT(e->kind == Expression::Literal);
        Type* t = deref(e->type);
        quint8 ch;
        if( t->form == BasicType::CHAR )
            ch = e->val.toUInt();
        else
            ch = (quint8)e->val.toByteArray()[0];
        const int res = ctx.back().buySlots(1);
        bc.KSET(res, ch, e->pos.packed());
        slotStack.push_back(res);
    }

    void emitRelationOp(Expression* e)
    {
        Type* lt = deref(e->lhs->type);
        Type* rt = deref(e->rhs->type);
        if( rt->form == BasicType::CHAR && e->lhs->isCharLiteral())
        {
            pushCharLiteral(e->lhs);
            lt = rt;
        }else
            emitExpression(e->lhs);
        if( lt->form == BasicType::CHAR && e->rhs->isCharLiteral())
        {
            pushCharLiteral(e->rhs);
            rt = lt;
        }else
            emitExpression(e->rhs);
        Q_ASSERT(slotStack.size() >= 2);
        const quint8 lhs = slotStack[slotStack.size()-2];
        const quint8 rhs = slotStack.back();
        const quint8 res = lhs;
        if( ( lt->isNumber() || lt->form == BasicType::CHAR || lt->form == Type::ConstEnum ) &&
                ( rt->isNumber() || rt->form == BasicType::CHAR || rt->form == Type::ConstEnum ) )
        {
            switch(e->kind)
            {
            case Expression::Eq:
                bc.ISEQ(lhs, rhs, e->pos.packed() );
                break;
            case Expression::Neq:
                bc.ISNE(lhs, rhs, e->pos.packed() );
                break;
            case Expression::Lt:
                bc.ISLT(lhs, rhs, e->pos.packed() );
                break;
            case Expression::Leq:
                bc.ISLE(lhs, rhs, e->pos.packed() );
                break;
            case Expression::Gt:
                bc.ISGT(lhs, rhs, e->pos.packed() );
                break;
            case Expression::Geq:
                bc.ISGE(lhs, rhs, e->pos.packed() );
                break;
            }
            jumpTrueFalse(res, e->pos);
        }else if( lt->isReference() || lt->form == BasicType::Nil || lt->isSet() || lt->isBoolean() )
        {
            switch(e->kind)
            {
            case Expression::Eq:
                bc.ISEQ(lhs, rhs, e->pos.packed() );
                break;
            case Expression::Neq:
                bc.ISNE(lhs, rhs, e->pos.packed() );
                break;
            }
            jumpTrueFalse(res, e->pos);
        }else if( lt->form == BasicType::StrLit || lt->form == BasicType::STRING )
        {
            // lhs and rhs are either lua strings or lua ffi CharArray
            int op = 0;
            switch(e->kind)
            {
            case Expression::Eq:
                op = 1;
                break;
            case Expression::Neq:
                op = 2;
                break;
            case Expression::Lt:
                op = 3;
                break;
            case Expression::Leq:
                op = 4;
                break;
            case Expression::Gt:
                op = 5;
                break;
            case Expression::Geq:
                op = 6;
                break;
            }
            const int tmp = ctx.back().buySlots(4,true);
            fetchLnlibMember(tmp,17, e->pos); // stringRelOp
            bc.MOV(tmp+1, lhs, e->pos.packed() );
            bc.MOV(tmp+2, rhs, e->pos.packed() );
            bc.KSET(tmp+3, op, e->pos.packed() );
            bc.CALL(tmp,1,3, e->pos.packed());
            bc.MOV(res,tmp, e->pos.packed());
            ctx.back().sellSlots(tmp,4);
        }else
            Q_ASSERT(false);
        releaseSlot(); // remove rhs, keep lhs as result
    }

    void emitLogicOp(Expression* e)
    {
        emitExpression(e->lhs);
        if( e->kind == Expression::And )
            bc.ISF(slotStack.back(),e->pos.packed());
        else
            bc.IST(slotStack.back(),e->pos.packed());
        emitJMP(0,e->pos.packed());
        const quint32 pc1 = bc.getCurPc();
        releaseSlot();
        emitExpression(e->rhs);
        if( e->kind == Expression::And )
            bc.ISF(slotStack.back(),e->pos.packed());
        else
            bc.IST(slotStack.back(),e->pos.packed());
        emitJMP(0,e->pos.packed());
        const quint32 pc2 = bc.getCurPc();
        if( e->kind == Expression::And )
            bc.KSET(slotStack.back(), true, e->pos.packed() );
        else
            bc.KSET(slotStack.back(), false, e->pos.packed() );
        emitJMP(0,e->pos.packed());
        const quint32 pc3 = bc.getCurPc();
        bc.patch(pc1);
        bc.patch(pc2);
        if( e->kind == Expression::And )
            bc.KSET(slotStack.back(), false, e->pos.packed() );
        else
            bc.KSET(slotStack.back(), true, e->pos.packed() );
        bc.patch(pc3);
    }

    void setOp( quint8 res, int op, const RowCol& loc )
    {
        const int tmp = ctx.back().buySlots(3,true);
        fetchLnlibMember(tmp,op,loc);
        bc.MOV(tmp+1, slotStack[slotStack.size()-2], loc.packed() );
        bc.MOV(tmp+2, slotStack.back(), loc.packed() );
        bc.CALL(tmp,1,2,loc.packed());
        bc.MOV(res,tmp,loc.packed());
        ctx.back().sellSlots(tmp,3);
    }

    void emitCharToStr(quint8 ch, const RowCol& loc )
    {
        const int tmp = ctx.back().buySlots(2,true);
        fetchLnlibMember(tmp, 55, loc ); // string.char
        bc.MOV(tmp + 1, ch, loc.packed() );
        bc.CALL(tmp,1,1,loc.packed());
        bc.MOV(ch,tmp, loc.packed());
        ctx.back().sellSlots(tmp,2);
    }

    void emitArithOp(Expression* e)
    {
        Type* lt = deref(e->lhs->type);

        emitExpression(e->lhs);
        quint8 lhs = slotStack.back();
        emitExpression(e->rhs);
        quint8 rhs = slotStack.back();
        quint8 res = lhs;
        if( lt->isNumber() )
        {
            switch(e->kind)
            {
            case Expression::Mul:
                bc.MUL(res, lhs, rhs, e->pos.packed());
                break;
            case Expression::Div: {
                    const int tmp = ctx.back().buySlots(3,true);
                    fetchLnlibMember(tmp,14,e->pos); // module.DIV
                    bc.MOV(tmp+1, lhs, e->pos.packed() );
                    bc.MOV(tmp+2, rhs, e->pos.packed() );
                    bc.CALL(tmp,1,2,e->pos.packed());
                    bc.MOV(res,tmp,e->pos.packed());
                    ctx.back().sellSlots(tmp,3);
                    break;
                }
            case Expression::Fdiv:
                bc.DIV(res, lhs, rhs, e->pos.packed());
                break;
            case Expression::Mod:
                if( lt->isInteger() )
                {
                    const int tmp = ctx.back().buySlots(3,true);
                    fetchLnlibMember(tmp,15,e->pos); // module.MOD
                    bc.MOV(tmp+1, lhs, e->pos.packed() );
                    bc.MOV(tmp+2, rhs, e->pos.packed() );
                    bc.CALL(tmp,1,2,e->pos.packed());
                    bc.MOV(res,tmp,e->pos.packed());
                    ctx.back().sellSlots(tmp,3);
                }else
                    bc.MOD(res, lhs, rhs, e->pos.packed());
                break;
            case Expression::Add:
                bc.ADD(res, lhs, rhs, e->pos.packed());
                break;
            case Expression::Sub:
                bc.SUB(res, lhs, rhs, e->pos.packed());
                break;
            default:
                Q_ASSERT(false);
            }
        }else if( lt->isSet() )
        {
            switch(e->kind)
            {
            case Expression::Mul:
                setOp( res, 19, e->pos ); // bit.band
                break;
            case Expression::Fdiv:
                setOp( res, 20, e->pos ); // module.setDiv
                break;
            case Expression::Add:
                setOp( res, 12, e->pos ); // bit.bor
                break;
            case Expression::Sub:
                setOp( res, 18, e->pos ); // module.setSub
                break;
            default:
                Q_ASSERT(false);
            }
        }else if( lt->form == BasicType::StrLit || lt->form == BasicType::CHAR || lt->form == BasicType::STRING )
        {
            if( lt->form == BasicType::CHAR )
                emitCharToStr(lhs, e->pos);
            Type* rt = deref(e->rhs->type);
            if( rt->form == BasicType::CHAR )
                emitCharToStr(rhs, e->pos);
            bc.CAT(res, lhs, rhs, e->pos.packed() );
        }
        releaseSlot(); // rhs, lhs is used as result
    }

    void emitLvalueToSlot( quint8 slot, const Lvalue& acc, Type* hashMapElem, const RowCol& loc )
    {
        switch( acc.kind )
        {
        case Lvalue::Slot:
                bc.MOV(slot, acc.slot, loc.packed() );
            break;
        case Lvalue::TableIdx:
            emitGetTableByIndex(slot, acc.slot, acc.index, loc );
            break;
        case Lvalue::TableIdxSlot:
            bc.TGET(slot, acc.slot, acc.index, loc.packed() );
            if( hashMapElem )
                emitFixHashmapElem(slot, hashMapElem, loc);
            break;
        default:
            Q_ASSERT( false );
        }
    }

    static int builtinToMagic(int bi)
    {
        switch(bi)
        {
        case Builtin::BITAND:
            return 19; // bit.band
        case Builtin::BITASR:
            return 36; // bit.arshift
        case Builtin::BITOR:
            return 12; // bit.bor
        case Builtin::BITSHL:
            return 35; // bit.lshift
        case Builtin::BITSHR:
            return 57; // bit.rshift
        case Builtin::BITXOR:
            return 41; // bit.bxor
        case Builtin::BITNOT:
            return 11; // bit.bnot
        case Builtin::ABS:
            return 34; // module.abs
        case Builtin::FLOOR:
            return 38; // module.floor
        case Builtin::ODD:
            return 33; // module.ODD
        case Builtin::STRLEN:
            return 24; // module.strlen
        case Builtin::TOSTRING:
            return 63; // module.tostring
        case Builtin::ASSERT:
            return 29;
        case Builtin::EXCL:
            return 30; // module.removeElemFromSet
        case Builtin::INCL:
            return 9; // addElemToSet
        case Builtin::PRINT:
            return 56; // module.print
        case Builtin::PRINTLN:
            return 25; // module.println
        case Builtin::LEN:
            return 58; // module.arraylen
        case Builtin::CAP:
        case Builtin::BITS:
        case Builtin::CAST:
        case Builtin::CHR:
        case Builtin::DEFAULT:
        case Builtin::FLT:
        case Builtin::GETENV:
        case Builtin::MAX:
        case Builtin::MIN:
        case Builtin::ORD:
            break;
        case Builtin::COPY:
        case Builtin::DEC:
        case Builtin::HALT:
        case Builtin::INC:
        case Builtin::NEW:
        case Builtin::PCALL:
        case Builtin::RAISE:
        case Builtin::SETENV:
            break;
        }
        return 0;
    }

    void emitBuiltinN(int op, Expression* call, int count, int res = -1)
    {
        ExpList args = Expression::getList(call->rhs);
        Q_ASSERT(args.size() == count);
        const quint8 tmp = ctx.back().buySlots(1+count, true);
        fetchLnlibMember(tmp,builtinToMagic(op),call->pos);

        for( int i = 0; i < args.size(); i++ )
        {
            emitExpression(args[i]);
            bc.MOV(tmp+i+1, slotStack.back(), call->pos.packed() );
            releaseSlot();
        }

        bc.CALL(tmp, res >= 0 ? 1 : 0,count, call->pos.packed());
        if( res >= 0 )
            bc.MOV(res,tmp, call->pos.packed());
        ctx.back().sellSlots(tmp,1+count);
    }

    void emitIncDec(quint8 res, Expression* var, Expression* by, bool isInc, const RowCol& pos)
    {
        // INC(v) integer type v := v + 1
        // INC(v, n) v, n: integer type v := v + n

        Lvalue v = lvalue(var);
        emitLvalueToSlot(res, v, 0, pos);
        quint8 off;
        if( by )
        {
            emitExpression(by);
            off = slotStack.back();
        }else
        {
            off = ctx.back().buySlots(1);
            bc.KSET(off, 1, pos.packed());
            slotStack.push_back(off);
        }
        if( isInc )
            bc.ADD(res, res, off, pos.packed());
        else
            bc.SUB(res, res, off, pos.packed());
        releaseSlot();
        emitSlotToLvalue(v,res,pos);
        releaseLvalue(v);
    }

    void emitClip(quint8 to, quint8 from, int bits, const RowCol& pos)
    {
        const quint8 tmp = ctx.back().buySlots(3, true);
        fetchLnlibMember(tmp,19,pos); // bit.band
        bc.MOV(tmp+1, from, pos.packed() );
        bc.KSET(tmp+2, (1 << bits) - 1, pos.packed());
        bc.CALL(tmp, 1 ,2, pos.packed());
        bc.MOV(to,tmp, pos.packed());
        ctx.back().sellSlots(tmp,3);
    }

    void emitBuiltin(Declaration* proc, Expression* call, quint8 res)
    {
        switch( proc->id )
        {
        case Builtin::BITAND:
        case Builtin::BITASR:
        case Builtin::BITOR:
        case Builtin::BITSHL:
        case Builtin::BITSHR:
        case Builtin::BITXOR:
            emitBuiltinN(proc->id, call, 2, res);
            break;
        case Builtin::ODD:
        case Builtin::ABS:
        case Builtin::FLOOR:
        case Builtin::BITNOT:
        case Builtin::STRLEN:
        case Builtin::TOSTRING:
            emitBuiltinN(proc->id, call, 1, res);
            break;
        case Builtin::ORD:
            emitExpression(call->rhs);
            if( deref(call->rhs->type)->form == BasicType::BOOLEAN )
            {
                bc.IST(slotStack.back(),call->pos.packed());
                emitJMP(0,call->pos.packed());
                const quint32 pc1 = bc.getCurPc();
                bc.KSET(res, 0, call->pos.packed() );
                emitJMP(0,call->pos.packed());
                const quint32 pc2 = bc.getCurPc();
                bc.patch(pc1);
                bc.KSET(res, 1, call->pos.packed() );
                bc.patch(pc2);
            }else
                bc.MOV(res,slotStack.back(),call->pos.packed() );
            releaseSlot();
            break;
        case Builtin::BITS:
            emitExpression(call->rhs);
            emitClip(res, slotStack.back(), 32, call->pos);
            releaseSlot();
            break;
        case Builtin::FLT:
            emitExpression(call->rhs);
            bc.MOV(res,slotStack.back(),call->pos.packed() );
            releaseSlot();
            break;
        case Builtin::CLIP:
        case Builtin::CHR:
            emitExpression(call->rhs);
            emitClip(res, slotStack.back(), 8, call->pos);
            releaseSlot();
            break;
        case Builtin::DEFAULT:
            Q_ASSERT(false); // only called at compile time
            emitInitializer(res, call->rhs->type, call->pos);
            break;
        case Builtin::ASSERT:
            emitBuiltinN(proc->id, call, 3);
            break;
        case Builtin::PRINT:
        case Builtin::PRINTLN:
            emitBuiltinN(proc->id, call, 1);
            break;
        case Builtin::MAX:
            Q_ASSERT(false); // only called at compile time
            bc.KSET(res, BasicType::getMax(deref(call->rhs->type)->form), call->pos.packed());
            // TODO: version with two numeric args
            break;
        case Builtin::MIN:
            Q_ASSERT(false); // only called at compile time
            bc.KSET(res, BasicType::getMin(deref(call->rhs->type)->form), call->pos.packed());
            // TODO: version with two numeric args
            break;
        case Builtin::EXCL:
        case Builtin::INCL:  {
                // EXCL(v, x) v: SET; x: integer type v := v - {x}
                // INCL(v, x) v: SET; x: integer type v := v + {x}
                emitBuiltinN(proc->id,call, 2, res);
                Lvalue v = lvalue(call->rhs);
                emitSlotToLvalue(v,res,call->pos);
                releaseLvalue(v);
                break;
            }
        case Builtin::CAST:
            // CAST(T,x) T:enumeration type x:ordinal number enumeration type
            emitExpression(call->rhs->next);
            bc.MOV(res,slotStack.back(),call->pos.packed());
            releaseSlot();
            break;
        case Builtin::DEC:
        case Builtin::INC:
            emitIncDec(res, call->rhs, call->rhs->next, proc->id == Builtin::INC, call->pos);
            break;
        case Builtin::NEW: {
                Type* t = deref(call->rhs->type);
                switch( t->form )
                {
                case Type::Record:
                    emitCreateRecord(res, t, call->pos);
                    break;
                case Type::Array:
                    if( t->len )
                        emitCreateArray(res, t, -1, call->pos );
                    else
                    {
                        Q_ASSERT( call->rhs->next );
                        emitExpression(call->rhs->next);
                        emitCreateArray(res, t, slotStack.back(), call->pos );
                        releaseSlot();
                    }
                    break;
                case Type::HashMap:
                    bc.TNEW(res, 0, 0, call->pos.packed());
                    break;
                }
                Lvalue v = lvalue(call->rhs);
                emitSlotToLvalue(v,res,call->pos);
                releaseLvalue(v);
                break;
            }
        case Builtin::LEN: {
                // LEN(v) v: array
                //        v: string length of string (including the terminating 0X)
                Type* t = deref(call->rhs->type);
                switch( t->form )
                {
                case BasicType::STRING:
                case BasicType::StrLit:
                    emitBuiltinN(proc->id, call, 1, res);
                    break;
                case Type::Array:
                    if( t->len )
                        bc.KSET(res, t->len, call->pos.packed() );
                    else
                        emitBuiltinN(proc->id, call, 1, res);
                    break;
                }
                break;
            }
        case Builtin::KEYS: {
                emitExpression(call->rhs);
                const quint8 tmp = ctx.back().buySlots(3, true);
                fetchLnlibMember(tmp,64,call->pos); // module.keys
                bc.MOV(tmp+1, slotStack.back(), call->pos.packed() );
                releaseSlot();
                Type* base = deref(deref(call->rhs->type)->base);
                bc.KSET(tmp+2, base->form == BasicType::CHAR || base->form == BasicType::BYTE, call->pos.packed());
                bc.CALL(tmp, 1 ,2, call->pos.packed());
                bc.MOV(res,tmp, call->pos.packed());
                ctx.back().sellSlots(tmp,3);
                break;
            }
        case Builtin::COPY: {
                Type* rt;
                Type* lt = 0;
                if( call->rhs->next )
                {
                    emitExpression(call->rhs->next);
                    rt = deref(call->rhs->next->type);
                    lt = deref(call->rhs->type);
                }else
                {
                    emitExpression(call->rhs);
                    rt = deref(call->rhs->type);
                }
                const int tmp = ctx.back().buySlots(3,true);
                if( rt->form < BasicType::Max ) // string, literals
                {
                    fetchLnlibMember(tmp, 7, call->pos ); // module.stringToCharArray
                    bc.MOV(tmp + 1, slotStack.back(), call->pos.packed() );
                    releaseSlot();
                    bc.KSET(tmp+2, lt ? lt->len : 0, call->pos.packed() );
                    bc.CALL(tmp,1,1,call->pos.packed());
                    bc.MOV(res,tmp, call->pos.packed());
                }else
                {
                    fetchLnlibMember(tmp,60,call->pos); // module.clone
                    bc.MOV(tmp+1, slotStack.back(), call->pos.packed() );
                    releaseSlot();
                    if( rt->form == Type::Record )
                        bc.KSET(tmp+2, rt->countAllocRecordMembers(true).first, call->pos.packed());
                    else
                        bc.KNIL(tmp+2,1,call->pos.packed());
                    bc.CALL(tmp, 1,2, call->pos.packed());
                    bc.MOV(res, tmp, call->pos.packed());
                }
                ctx.back().sellSlots(tmp,3);
                if( call->rhs->next )
                {
                    Lvalue v = lvalue(call->rhs);
                    emitSlotToLvalue(v,res,call->pos);
                    releaseLvalue(v);
                }
                break;
            }
        case Builtin::TRAP: {
                const quint8 tmp = ctx.back().buySlots(1,true);
                bc.GGET(tmp, "TRAP", call->pos.packed());
                bc.CALL(tmp,0,0,call->pos.packed());
                ctx.back().sellSlots(tmp,1);
            }
            break;
        case Builtin::TRAPIF: {
                emitExpression(call->rhs);
                Q_ASSERT( !slotStack.isEmpty() );

                const quint8 tmp = ctx.back().buySlots(2,true);
                bc.GGET(tmp, "TRAP", call->pos.packed());
                bc.MOV(tmp+1,slotStack.back(),call->pos.packed());
                bc.CALL(tmp,0,1,call->pos.packed());
                ctx.back().sellSlots(tmp,2);
                releaseSlot();
            }
            break;
            // TODO:
        case Builtin::CAP:
        case Builtin::GETENV:
            break;
        case Builtin::HALT:
        case Builtin::PCALL:
        case Builtin::RAISE:
        case Builtin::SETENV:
            break;
        }
    }

    void emitGetClassObject(quint8 to, quint8 instance, const RowCol& loc)
    {
        const int tmp = ctx.back().buySlots(2,true);
        fetchLnlibMember( tmp, 40, loc ); // getmetatable
        bc.MOV( tmp+1, instance, loc.packed() ); // this
        bc.CALL( tmp, 1, 1, loc.packed() ); // resulting tmp is class of this
        bc.MOV( to, tmp, loc.packed() );
        ctx.back().sellSlots(tmp,2);
    }

    void emitCallOp(Expression* e)
    {
        const int res = ctx.back().buySlots(1); // always allocate a result slot, even for proper procs

        Expression* lhs = e->lhs;
        bool supercall = false;
        if( lhs && lhs->kind == Expression::Super )
        {
            supercall = true;
            lhs = lhs->lhs;
        }
        emitExpression(lhs);
        const quint8 procSlot = slotStack.back();

        Declaration* proc = lhs->val.value<Declaration*>();

        Type* returnType = deref(lhs->type);
        Type* procType = 0;
        bool isDelegate = false;
        if( proc && proc->kind == Declaration::Builtin )
        {
            releaseSlot(); // we dont use the procSlot
            emitBuiltin( proc, e, res );
            slotStack.push_back(res);
            return;
        }else if( proc && proc->kind != Declaration::Procedure )
        {
            proc = 0;
            // so it must be a proc type variable
            Q_ASSERT(returnType->form == Type::Proc);
            isDelegate = returnType->receiver;
            procType = returnType;
            returnType = deref(returnType->base);
        }else if( proc == 0 )
        {
            Q_ASSERT(returnType->form == Type::Proc);
            isDelegate = returnType->receiver;
            procType = returnType;
            returnType = deref(returnType->base);
        }
        Q_ASSERT( proc || procType );

        DeclList formals = proc ? proc->getParams(true) : procType->subs;
        ExpList actuals = Expression::getList(e->rhs);
        Q_ASSERT( actuals.size() >= formals.size() );

        QVector<Lvalue> accs(formals.size());
        int varCount = 0; // number of true var params which have to be returned by the function
        for( int i = 0; i < formals.size(); i++ )
        {
            if( formals[i]->varParam )
            {
                varCount++;
                accs[i] = lvalue( actuals[i] );
            }
        }

        bool isBound = false;
        if( proc && proc->mode == Declaration::Receiver )
            isBound = true;

        const int funcCount = 1;
        // we always assume a return value if there are vars to be returned even if there isn't one
        const int retCount = returnType->form == BasicType::NoType && varCount == 0 ? 0 : 1;

        const int thisCount = ( isBound || isDelegate ? 1 : 0 );
        const int argCount = actuals.size();

        const int slot = ctx.back().buySlots( funcCount + thisCount + argCount, true ); // Allocate the slots for the call

        if( isDelegate )
        {
            // a delegate value is a table with two slots: 0 for this and 1 for the method
            bc.TGETi( slot, procSlot, 1, e->pos.packed() ); // method
            bc.TGETi( slot+funcCount, procSlot, 0, e->pos.packed() ); // this
        }else if( isBound )
        {
            // "this" is on the stack instead of proc, so we first have to fetch proc (via metatable)
            emitGetClassObject(slot, procSlot, e->pos);
            if( supercall )
                emitGetClassObject(slot, slot, e->pos);
            emitGetTableByIndex(slot, slot, proc->id, e->pos);
            // then we move "this" to the first parameter slot
            bc.MOV(slot+funcCount, procSlot, e->pos.packed());
        }else
            bc.MOV( slot, procSlot, e->pos.packed() ); // procedure
        releaseSlot();

        for( int i = 0; i < formals.size(); i++ )
        {
            const int off = funcCount + thisCount + i;
            if( accs[i].kind != Lvalue::Invalid )
            {
                Q_ASSERT( formals[i]->varParam );
                Type* h = deref(formals[i]->type);
                if( h->form == Type::HashMap )
                    h = h->expr->type;
                else
                    h = 0;
                emitLvalueToSlot(slot+off, accs[i], h, actuals[i]->pos );
            }else
            {
                // here all by val (i.e. !var)
                emitExpression(actuals[i]);
                prepareRhs( formals[i]->type, actuals[i], actuals[i]->pos );
                bc.MOV(slot+off, slotStack.back(), actuals[i]->pos.packed() );
                releaseSlot();
            }
        }

        // pass varargs if present
        for( int i = formals.size(); i < actuals.size(); i++ )
        {
            emitExpression(actuals[i]);
            const int off = funcCount + thisCount + i;
            bc.MOV(slot+off, slotStack.back(), actuals[i]->pos.packed() );
            releaseSlot();
        }

        bc.CALL( slot, retCount + varCount, thisCount + argCount, e->pos.packed() );
        if( retCount )
            bc.MOV(res, slot, e->pos.packed() );

        // handle returned vars
        int off = retCount;
        foreach( const Lvalue& acc, accs )
        {
            if( acc.kind == Lvalue::Invalid )
                continue;
            Q_ASSERT( retCount != 0 );
            emitSlotToLvalue(acc,slot+off,e->pos);
            off++;
            releaseLvalue(acc);
        }

        ctx.back().sellSlots( slot, funcCount + thisCount + argCount );
        slotStack.push_back(res);
    }

    void emitRecordConstructor(Expression* e)
    {
        quint8 res = ctx.back().buySlots(1);
        emitCreateRecord(res,e->type,e->pos);
        int index = 0;
        Expression* c = e->rhs;
        while( c )
        {
            if( c->kind == Expression::KeyValue )
            {
                Declaration* field = c->lhs->val.value<Declaration*>();
                index = field->id;
                emitExpression(c->rhs);
                emitSetTableByIndex(slotStack.back(),res, index, c->pos);
                releaseSlot();
            }else
            {
                emitExpression(c);
                emitSetTableByIndex(slotStack.back(),res, index, c->pos);
                releaseSlot();
            }
            index++;
            c = c->next;
        }
        slotStack.push_back(res);
    }

    void emitArrayConstructor(Expression* e)
    {
        quint8 res = ctx.back().buySlots(1);
        // NOTE: the validator changes open to fix array types for constructors
        Type* array = deref(e->type);
        emitCreateArray(res,array, -1,e->pos);
        Q_ASSERT(array->len); // the validator converted all dynamic arrays to fix len
        int index = 0;
        Expression* c = e->rhs;
        while( c )
        {
            if( c->kind == Expression::KeyValue )
            {
                index = c->lhs->val.toLongLong();
                emitExpression(c->rhs);
                emitSetTableByIndex(slotStack.back(),res, index, c->pos);
                releaseSlot();
            }else
            {
                emitExpression(c);
                emitSetTableByIndex(slotStack.back(),res, index, c->pos);
                releaseSlot();
            }
            index++;
            c = c->next;
        }
        slotStack.push_back(res);
    }

    void emitHashmapConstructor(Expression* e)
    {
        quint8 res = ctx.back().buySlots(1);
        bc.TNEW( res, 0, 0, e->pos.packed() );
        Q_ASSERT(deref(e->type)->len); // the validator converted all dynamic arrays to fix len
        Expression* c = e->rhs;
        while( c )
        {
            if( c->kind == Expression::KeyValue )
            {
                emitExpression(c->lhs);
                emitExpression(c->rhs);
                bc.TSET(slotStack.back(), res, slotStack[slotStack.size()-2], c->pos.packed());
                releaseSlot();
                releaseSlot();
            }else
                Q_ASSERT(false);
            c = c->next;
        }
        slotStack.push_back(res);
    }

    void emitSetConstructor(Expression* e)
    {
        const int res = ctx.back().buySlots(1);

        bc.KSET(res, 0, e->pos.packed() );

        const quint8 addElemToSet = ctx.back().buySlots(1);
        fetchLnlibMember(addElemToSet,9,e->pos); // module.addElemToSet
        const quint8 addRangeToSet = ctx.back().buySlots(1);
        fetchLnlibMember(addRangeToSet,10,e->pos); // module.addRangeToSet

        Expression* c = e->rhs;
        while( c )
        {
            if( c->kind == Expression::Range )
            {
                const int tmp = ctx.back().buySlots(4,true);
                bc.MOV(tmp, addRangeToSet, c->lhs->pos.packed() );
                bc.MOV(tmp+1, res, c->lhs->pos.packed() );
                emitExpression(c->lhs);
                bc.MOV(tmp+2, slotStack.back(), c->lhs->pos.packed() );
                releaseSlot();
                emitExpression( c->rhs );
                bc.MOV(tmp+3, slotStack.back(), c->rhs->pos.packed() );
                releaseSlot();
                bc.CALL(tmp,1,3, c->rhs->pos.packed() );
                bc.MOV(res, tmp, c->rhs->pos.packed() );
                ctx.back().sellSlots(tmp,4);
            }else
            {
                const int tmp = ctx.back().buySlots(3,true);
                bc.MOV(tmp, addElemToSet, e->pos.packed() );
                bc.MOV(tmp+1, res, e->pos.packed() );
                emitExpression(c);
                bc.MOV(tmp+2, slotStack.back(), c->pos.packed() );
                releaseSlot();
                bc.CALL(tmp,1,2, e->pos.packed() );
                bc.MOV(res, tmp, e->pos.packed() );
                ctx.back().sellSlots(tmp,3);
            }
            c = c->next;
        }

        ctx.back().sellSlots(addElemToSet);
        ctx.back().sellSlots(addRangeToSet);
        slotStack.push_back(res);
    }

    void emitConstructor(Expression* e)
    {
        Type* t = deref(e->type);
        switch( t->form )
        {
        case Type::Record:
            emitRecordConstructor(e);
            break;
        case Type::Array:
            emitArrayConstructor(e);
            break;
        case Type::HashMap:
            emitHashmapConstructor(e);
            break;
        case BasicType::SET:
            emitSetConstructor(e);
            break;
        default:
            Q_ASSERT(false);
        }
    }

    void emitFixHashmapElem(quint8 val, Type* base, const RowCol& pos)
    {
        base = deref(base);
        if( !base->isReference() )
        {
            // if hashmap check if lhs is nil and possibly replace by default
            const int tmp = ctx.back().buySlots(1);
            bc.KNIL(tmp, 1, pos.packed());
            bc.ISNE(tmp, val, pos.packed() );
            emitJMP(0, pos.packed());
            const quint32 pc1 = bc.getCurPc();
            emitInitializer(tmp, base, pos);
            bc.MOV(val, tmp, pos.packed() );
            bc.patch(pc1);
            ctx.back().sellSlots(tmp);
        }
    }

    void emitExpression(Expression* e)
    {
        switch(e->kind)
        {
        case Expression::Plus:
            emitExpression(e->lhs);
            break;
        case Expression::Minus:
            emitExpression(e->lhs);
            if( deref(e->type)->form == BasicType::SET )
            {
                const int tmp = ctx.back().buySlots(2,true);
                fetchLnlibMember(tmp,11,e->pos); // bit.bnot
                bc.MOV(tmp+1,slotStack.back(),e->pos.packed());
                bc.CALL(tmp,1,1,e->pos.packed());
                bc.MOV(slotStack.back(),tmp,e->pos.packed());
                ctx.back().sellSlots(tmp,2);
            }else
                bc.UNM(slotStack.back(),slotStack.back(),e->pos.packed());
            break;
        case Expression::Not:
            emitExpression(e->lhs);
            bc.NOT(slotStack.back(),slotStack.back(),e->pos.packed());
            break;
        case Expression::Eq:
        case Expression::Neq:
        case Expression::Lt:
        case Expression::Leq:
        case Expression::Gt:
        case Expression::Geq:
            emitRelationOp(e);
            break;
        case Expression::In: {
                emitExpression(e->lhs);
                quint8 lhs = slotStack.back();
                emitExpression(e->rhs);
                setOp( lhs, 21, e->pos ); // module.setTest
                releaseSlot();
                break;
            }
        case Expression::Is: {
                emitExpression(e->lhs);
                quint8 lhs = slotStack.back();
                emitExpression(e->rhs);
                quint8 rhs = slotStack.back();
                emitIsOp(lhs, rhs, lhs, e->pos);
                releaseSlot();
                break;
            }
        case Expression::Add:
        case Expression::Sub:
        case Expression::Mul:
        case Expression::Fdiv:
        case Expression::Div:
        case Expression::Mod:
            emitArithOp(e);
            break;
        case Expression::Or:
        case Expression::And:
            emitLogicOp(e);
            break;
        case Expression::DeclRef: {
                Declaration* d = e->val.value<Declaration*>();
                const int res = ctx.back().buySlots(1);
                switch(d->kind)
                {
                case Declaration::Builtin:
                    // NOP
                    break;
                case Declaration::VarDecl:
                case Declaration::Procedure:
                case Declaration::TypeDecl:
                case Declaration::ConstDecl:
                    fetchModule(d->getModule(), res, e->pos );
                    emitGetTableByIndex(res, res, d->id, e->pos);
                    break;
                case Declaration::LocalDecl:
                case Declaration::ParamDecl:
                    bc.MOV(res,d->id, e->pos.packed());
                    break;
                default:
                    Q_ASSERT(false);
                }
                slotStack.push_back(res);
                break;
            }
        case Expression::Select: {
                emitExpression(e->lhs);
                Declaration* d = e->val.value<Declaration*>();
                if( d->kind == Declaration::Procedure && d->mode == Declaration::Receiver )
                    ; // NOP: we keep the this value on the slot stack instead
                else
                    emitGetTableByIndex(slotStack.back(), slotStack.back(), d->id, e->pos);
                break;
            }
        case Expression::Index: {
                emitExpression(e->lhs);
                quint8 lhs = slotStack.back();
                emitExpression(e->rhs);
                quint8 rhs = slotStack.back();
                bc.TGET(lhs,lhs,rhs, e->pos.packed());
                releaseSlot();
                Type* lhsT = deref(e->lhs->type);
                if( lhsT->form == Type::HashMap )
                    emitFixHashmapElem(lhs, lhsT->base, e->pos);
               break;
            }
        case Expression::Call:
            emitCallOp(e);
            break;
        case Expression::Literal:
        case Expression::ConstVal: {
                const int res = ctx.back().buySlots(1);
                emitConstValue(res, e->val, e->type, e->pos);
                slotStack.push_back(res);
                break;
            }
        case Expression::Constructor:
            emitConstructor(e);
            break;
        case Expression::Cast:
            emitExpression(e->lhs);
            break;
        default:
            Q_ASSERT(false);
        }
        Q_ASSERT( !slotStack.isEmpty() );
    }

    void releaseSlot()
    {
        if( slotStack.isEmpty() )
            return; // error already reported
        ctx.back().sellSlots(slotStack.back());
        slotStack.pop_back();
    }

    inline void emitJMP( qint16 offset, quint32 line )
    {
        bc.JMP(ctx.back().pool.d_frameSize, offset, line );
    }

    inline void statementSequence(Statement* s)
    {
        while( s )
        {
            emitStatement(s);
            s = s->getNext();
        }
    }

    void emitIf(Statement* s)
    {
        emitExpression(s->rhs);
        bc.ISF( slotStack.back(), s->pos.packed());
        releaseSlot();

        emitJMP(0, s->pos.packed() );
        const quint32 afterFirst = bc.getCurPc();

        statementSequence(s->body);

        QList<quint32> afterEnd;
        emitJMP(0, s->pos.packed() );
        afterEnd << bc.getCurPc();

        bc.patch(afterFirst);
        while( s->getNext() && s->getNext()->kind == Statement::Elsif )
        {
            s = s->getNext();
            emitExpression(s->rhs);
            bc.ISF( slotStack.back(), s->pos.packed());
            releaseSlot();
            emitJMP(0, s->pos.packed() );
            const quint32 afterThen = bc.getCurPc();

            statementSequence(s->body);

            emitJMP(0, s->pos.packed() );
            afterEnd << bc.getCurPc();

            bc.patch(afterThen);
        }

        if( s->getNext() && s->getNext()->kind == Statement::Else )
        {
            s = s->getNext();
            statementSequence(s->body);
        }

        foreach( quint32 pc, afterEnd )
            bc.patch(pc);
    }

    void emitWhile(Statement* me)
    {
        bc.LOOP( ctx.back().pool.d_frameSize, 0, me->pos.packed() ); // loop
        const quint32 loopStart = bc.getCurPc();

        emitExpression(me->rhs);
        bc.ISF( slotStack.back(), me->pos.packed());
        releaseSlot();

        emitJMP(0, me->pos.packed() );
        const quint32 outOfLoop = bc.getCurPc();

        statementSequence(me->body);

        bc.patch(loopStart);
        emitJMP(loopStart - bc.getCurPc() - 2, me->pos.packed() ); // jump to loopStart

        bc.patch(outOfLoop);
    }

    void emitRepeat(Statement* me)
    {
        // could be substituted by primitive LOOP and IF statement
        bc.LOOP( ctx.back().pool.d_frameSize, 0, me->pos.packed() ); // repeat
        const quint32 loopStart = bc.getCurPc();

        statementSequence(me->body);

        emitExpression(me->rhs);
        bc.IST( slotStack.back(), me->pos.packed());
        releaseSlot();

        emitJMP(0, me->pos.packed() ); // if true jump to afterEnd
        const quint32 afterEnd = bc.getCurPc();

        bc.patch(loopStart);
        emitJMP(loopStart - bc.getCurPc() - 2, me->pos.packed() ); // if false jump to loopStart

        bc.patch( afterEnd );
    }

    void emitLoop(Statement* me)
    {
        bc.LOOP( ctx.back().pool.d_frameSize, 0, me->pos.packed() ); // loop
        const quint32 loopStart = bc.getCurPc();

        statementSequence(me->body);

        bc.patch(loopStart);
        emitJMP(loopStart - bc.getCurPc() - 2, me->pos.packed() ); // jump to loopStart

        foreach( quint32 pc, exitJumps )
            bc.patch(pc);
        exitJumps.clear();
    }

    void emitIsOp(quint8 lhs, quint8 rhs, quint8 to, const RowCol& pos)
    {
        // lhs IS rhs
        const int tmp = ctx.back().buySlots(3,true);
        fetchLnlibMember(tmp,23, pos); // module.is_a
        bc.MOV(tmp+1, lhs, pos.packed());
        bc.MOV(tmp+2, rhs, pos.packed() );
        bc.CALL(tmp,1,2, pos.packed());
        bc.MOV(to, tmp, pos.packed());
        ctx.back().sellSlots(tmp,3);
    }

    void emitTypeCase( Statement* s )
    {
        const int lhs = ctx.back().buySlots(1);
        fetchClass(lhs, deref(s->rhs->type), s->rhs->pos );

        QList<int> breaks;
        while(s->getNext() && s->getNext()->kind == Statement::CaseLabel )
        {
            s = s->getNext();
            const int rhs = ctx.back().buySlots(1);
            fetchClass(rhs, deref(s->rhs->type), s->rhs->pos );
            emitIsOp(lhs, rhs, rhs, s->rhs->pos);
            bc.ISF(rhs, s->rhs->pos.packed() ); // if true run body, i.e. if false goto next case
            emitJMP(0, s->pos.packed() );
            const int nextCase = bc.getCurPc();
            ctx.back().sellSlots(rhs);

            statementSequence(s->body);

            // now we're done, jump out of the case statement
            emitJMP(0, s->pos.packed() );
            breaks << bc.getCurPc();

            bc.patch(nextCase);
        }

        if( s->getNext() && s->getNext()->kind == Statement::Else )
        {
            s = s->getNext();
            statementSequence(s->body);
        }

        foreach( quint32 pc, breaks )
            bc.patch(pc);

        ctx.back().sellSlots(lhs);
    }

    Lvalue lvalue( Expression* desig )
    {
        Lvalue acc;

        switch( desig->kind )
        {
        case Expression::Select: {
                emitExpression(desig->lhs);
                acc.slot = slotStack.back();
                acc.disposeSlot = 1;
                slotStack.pop_back();
                acc.index = desig->val.value<Declaration*>()->id;
                acc.kind = Lvalue::TableIdx;
                break;
            }
        case Expression::Index: {
                emitExpression(desig->lhs);
                acc.slot = slotStack.back();
                acc.disposeSlot = 1;
                slotStack.pop_back();
                emitExpression(desig->rhs);
                acc.index = slotStack.back();
                slotStack.pop_back();
                acc.kind = Lvalue::TableIdxSlot;
                acc.disposeIndex = 1;
                break;
            }
        case Expression::DeclRef: {
                Declaration* d = desig->val.value<Declaration*>();
                switch( d->kind )
                {
                case Declaration::VarDecl: {
                        const int tmp = ctx.back().buySlots(1);
                        fetchModule(d->getModule(),tmp,desig->pos);
                        acc.slot = tmp;
                        acc.disposeSlot = 1;
                        acc.index = d->id;
                        acc.kind = Lvalue::TableIdx;
                        break;
                    }
                case Declaration::LocalDecl:
                case Declaration::ParamDecl:
                    acc.slot = d->id;
                    acc.kind = Lvalue::Slot;
                    break;
                default:
                    Q_ASSERT(false);
                }
                break;
            }
        default:
            Q_ASSERT(false);
        }

        return acc;
    }

    void emitPlainCase( Statement* s )
    {
        emitExpression(s->rhs);
        const quint8 exp = slotStack.back();

        QList<int> breaks;
        while(s->getNext() && s->getNext()->kind == Statement::CaseLabel )
        {
            s = s->getNext();

            Expression* l = s->rhs;

            QList<int> doit;
            while( l )
            {
                if( l->kind == Expression::Range )
                {
                    const quint8 a = ctx.back().buySlots(1);
                    const quint8 b = ctx.back().buySlots(1);
                    const qint64 aa = l->lhs->getCaseValue();
                    const qint64 bb = l->rhs->getCaseValue();
                    bc.KSET(a,aa,l->lhs->pos.packed());
                    bc.KSET(b,bb,l->rhs->pos.packed());
                    if( aa <= bb )
                    {
                        bc.ISLT(exp,a,l->pos.packed()); // if exp < a goto next cond
                        emitJMP(0, l->pos.packed() );
                        const int nextCond = bc.getCurPc();
                        bc.ISLE(exp,b,l->pos.packed()); // if exp <= b do it else goto next cond
                        emitJMP(0, l->pos.packed() );
                        doit << bc.getCurPc();
                        bc.patch(nextCond);
                    }else
                    {
                        bc.ISLT(exp,b,l->pos.packed()); // if exp < b goto nextCond
                        emitJMP(0, l->pos.packed() );
                        const int nextCond = bc.getCurPc();
                        bc.ISLE(exp,a,l->pos.packed()); // if exp <= a do it else goto next cond
                        emitJMP(0, l->pos.packed() );
                        doit << bc.getCurPc();
                        bc.patch(nextCond);
                    }
                    ctx.back().sellSlots(a);
                    ctx.back().sellSlots(b);
                }else
                {
                    // we cannot use emitExpression(l) because "x" would be pushed as string!
                    const quint8 a = ctx.back().buySlots(1);
                    const qint64 aa = l->getCaseValue();
                    bc.KSET(a,aa,l->pos.packed());
                    bc.ISEQ(exp, a, l->pos.packed() );
                    emitJMP(0, l->pos.packed() );
                    doit << bc.getCurPc();
                    ctx.back().sellSlots(a);
                }
                l = l->next;
            }
            // if none of the cases applied, jump to nextCase
            emitJMP(0, s->pos.packed() );
            const int nextCase = bc.getCurPc();

            // we get here for the first case which fits exp
            foreach( quint32 pc, doit )
                bc.patch(pc);

            statementSequence(s->body);

            // now we're done, jump out of the case statement
            emitJMP(0, s->pos.packed() );
            breaks << bc.getCurPc();

            bc.patch(nextCase);
        }
        if( s->getNext() && s->getNext()->kind == Statement::Else )
        {
            s = s->getNext();
            statementSequence(s->body);
        }

        foreach( quint32 pc, breaks )
            bc.patch(pc);

        releaseSlot(); // exp
    }

    void releaseLvalue( const Lvalue& acc )
    {
        if( acc.disposeSlot )
            ctx.back().sellSlots(acc.slot);
        if( acc.disposeIndex )
            ctx.back().sellSlots(acc.index);
    }

    void emitSlotToLvalue( const Lvalue& acc, quint8 slot, const RowCol& loc )
    {
        switch( acc.kind )
        {
        case Lvalue::Slot:
            bc.MOV( acc.slot, slot, loc.packed() );
            break;
        case Lvalue::TableIdx:
            emitSetTableByIndex( slot, acc.slot, acc.index, loc );
            break;
        case Lvalue::TableIdxSlot:
            bc.TSET( slot, acc.slot, acc.index, loc.packed() );
            break;
        default:
            break;
        }
    }

    void emitAssig(Statement* s)
    {
        emitExpression(s->rhs);
        const quint8 rhs = slotStack.back();

        Lvalue lhs = lvalue( s->lhs );

        Type* rhsT = deref(s->rhs->type);
        if( lhs.kind == Lvalue::TableIdxSlot &&
                deref(s->lhs->lhs->type)->form == Type::HashMap &&
                !rhsT->isReference() )
        {
            // if rhs equals the default value, set key to nil to remove it
            const int tmp = ctx.back().buySlots(1);
            emitInitializer(tmp, rhsT, s->rhs->pos);
            bc.ISNE(tmp, slotStack.back(), s->rhs->pos.packed() );
            emitJMP(0, s->rhs->pos.packed());
            const quint32 pc1 = bc.getCurPc();
            bc.KNIL(slotStack.back(), 1, s->rhs->pos.packed() );
            bc.patch(pc1);
            ctx.back().sellSlots(tmp);
        }else
            prepareRhs(s->lhs->type, s->rhs, s->pos );
        emitSlotToLvalue(lhs, rhs, s->pos);
        releaseLvalue(lhs);

        releaseSlot(); // rhs
    }

    void emitInitializer( quint8 to, Type* t, const RowCol& loc)
    {
        t = deref(t);

        switch( t->form )
        {
        case BasicType::BOOLEAN:
            bc.KSET(to,false,loc.packed());
            break;
        case BasicType::CHAR:
        case BasicType::BYTE:
        case BasicType::INTEGER:
        case BasicType::SET:
        case Type::ConstEnum:
            bc.KSET(to,0,loc.packed());
            break;
        case BasicType::REAL:
            bc.KSET(to,0.0,loc.packed());
            break;
        case Type::Record:
        case Type::Array:
        case Type::HashMap:
        case Type::Proc:
            bc.KNIL(to,1,loc.packed());
            break;
        }
    }

    bool needsInitializer(Type* t)
    {
        t = deref(t);
        switch( t->form )
        {
        case BasicType::BOOLEAN:
        case BasicType::CHAR:
        case BasicType::BYTE:
        case BasicType::INTEGER:
        case BasicType::SET:
        case Type::ConstEnum:
        case BasicType::REAL:
            return true;
        }
        return false;
    }

    void prepareRhs(Type* lhsT, Expression* rhs, const RowCol& loc)
    {
        Q_ASSERT(rhs);
        lhsT = deref(lhsT);
        Type* rhsT = deref(rhs->type);

        if( lhsT->form == BasicType::CHAR && rhsT->form == BasicType::StrLit )
        {
            // convert len-1-string to char
#if 1
            // there is already the string on the stack; replace it by the char
            bc.KSET(slotStack.back(), (quint8)rhs->val.toByteArray()[0], loc.packed());
#else
            const int tmp = ctx.back().buySlots(2,true);
            fetchLnlibMember(tmp, 59, loc ); // string.byte
            bc.MOV(tmp + 1, slotStack.back(), loc.packed() );
            bc.CALL(tmp,1,1,loc.packed());
            bc.MOV(slotStack.back(),tmp, loc.packed());
            ctx.back().sellSlots(tmp,2);
#endif
        }else if( (lhsT->isDerefCharArray() && (rhsT->form == BasicType::StrLit || rhsT->form == BasicType::STRING ) )
                  || ( lhsT->isDerefByteArray() && rhsT->form == BasicType::ByteArrayLit ) )
        {
            // convert string to CharArray
            const int tmp = ctx.back().buySlots(3,true);
            fetchLnlibMember(tmp, 7, loc ); // module.stringToCharArray
            bc.MOV(tmp + 1, slotStack.back(), loc.packed() );
            bc.KSET(tmp+2, lhsT->len, loc.packed() );
            bc.CALL(tmp,1,1,loc.packed());
            bc.MOV(slotStack.back(),tmp, loc.packed());
            ctx.back().sellSlots(tmp,3);
        }else if( lhsT->form == BasicType::STRING && rhsT->isDerefCharArray() )
        {
            // convert CharArray to string
            const int tmp = ctx.back().buySlots(3,true);
            fetchLnlibMember(tmp, 61, loc ); // module.charArrayToString
            bc.MOV(tmp + 1, slotStack.back(), loc.packed() );
            bc.KSET(tmp+2, lhsT->len, loc.packed() );
            bc.CALL(tmp,1,1,loc.packed());
            bc.MOV(slotStack.back(),tmp, loc.packed());
            ctx.back().sellSlots(tmp,3);
        }else if( lhsT->form == Type::Proc && (rhs->kind == Expression::DeclRef || rhs->kind == Expression::Select) )
        {
            Declaration* d = rhs->val.value<Declaration*>();
            if( d->kind != Declaration::Procedure || d->mode != Declaration::Receiver )
                return;
            const int tmp = ctx.back().buySlots(1);
            // create a new delegate; slot 0 is this, slot 1 is method
            // "this" is already on the stack
            bc.TNEW(tmp,2,0,rhs->pos.packed());
            bc.TSETi(slotStack.back(),tmp,0,rhs->pos.packed()); // copy "this" to slot 0
            emitGetClassObject(slotStack.back(), slotStack.back(), rhs->pos);
            emitGetTableByIndex(slotStack.back(), slotStack.back(), d->id, rhs->pos); // fetch method
            bc.TSETi(slotStack.back(),tmp,1,rhs->pos.packed()); // copy method to slot 1
            bc.MOV(slotStack.back(), tmp,rhs->pos.packed());
            releaseSlot();
            slotStack.push_back(tmp);
        }
    }

    void emitReturn(Expression* what, const RowCol& loc)
    {
        Q_ASSERT( ctx.back().scope->kind == Declaration::Procedure );

        Declaration* proc = ctx.back().scope;
        DeclList params = proc->getParams();
        QVector<bool> accs(params.size());
        int varcount = 0;
        for( int i = 0; i < params.size(); i++)
        {
            accs[i] = params[i]->varParam;
            if( accs[i] )
                varcount++;
        }
        if( varcount )
        {
            const int tmp = ctx.back().buySlots( 1 + varcount );
            if( what )
            {
                emitExpression(what);
                prepareRhs(proc->type, what, what->pos);
                bc.MOV(tmp,slotStack.back(),loc.packed());
                releaseSlot();
            }else if( proc->type != 0 && proc->type->form != BasicType::NoType )
            {
                // a function with no body
                emitInitializer(tmp,proc->type,loc);
            }
            int pos = 1;
            for( int i = 0; i < params.size(); i++ )
            {
                if( accs[i] )
                {
                    bc.MOV(tmp+pos, params[i]->id, loc.packed() );
                    pos++;
                }
            }
            bc.RET(tmp,1+varcount,loc.packed());
            ctx.back().sellSlots( tmp, 1 + varcount );
        }else
        {
            if( what )
            {
                emitExpression(what);
                prepareRhs(proc->type, what, what->pos);
                bc.RET(slotStack.back(),1,loc.packed());
                releaseSlot();
            }else if( proc->type != 0 && proc->type->form != BasicType::NoType )
            {
                // a function with no body
                const int tmp = ctx.back().buySlots(1);
                emitInitializer(tmp,proc->type,loc);
                bc.RET(tmp,1,loc.packed()); // procedures always return one slot
                ctx.back().sellSlots(tmp);
            }else
            {
                bc.RET(loc.packed()); // if we combine this with the !pt->d_return.isNull() part and
                                        // always return an arg then OberonSystem crashes randomly
            }
        }
    }

    void emitFor(Statement* s)
    {
        emitAssig(s);

        Q_ASSERT(s->getNext() && s->getNext()->kind == Statement::ForToBy);
        Statement* me = s->getNext();
        const int to = ctx.back().buySlots(1);
        emitExpression(me->lhs);
        bc.MOV(to, slotStack.back(), me->lhs->pos.packed());
        releaseSlot();

        int step = 1;
        if( me->rhs )
            step = me->rhs->val.toLongLong();

        bc.LOOP( ctx.back().pool.d_frameSize, 0, me->pos.packed() ); // loop
        const quint32 loopStart = bc.getCurPc();

        emitExpression(s->lhs); // i
        if( step > 0 )
            bc.ISGT(slotStack.back(), to, me->pos.packed() );
        else
            bc.ISLT(slotStack.back(), to, me->pos.packed() );
        releaseSlot();

        emitJMP(0, me->pos.packed() );
        const quint32 outOfLoop = bc.getCurPc();

        statementSequence(s->body);

        const int tmp = ctx.back().buySlots(1);
        emitIncDec(tmp, s->lhs, me->rhs, true, me->pos);
        ctx.back().sellSlots(tmp);

        bc.patch(loopStart);
        emitJMP(loopStart - bc.getCurPc() - 2, me->pos.packed() ); // jump to loopStart

        bc.patch(outOfLoop);
        ctx.back().sellSlots(to);
    }

    void emitStatement(Statement* s)
    {
        switch(s->kind)
        {
        case Statement::Assig:
            emitAssig(s);
            break;
        case Statement::Call:
            emitExpression(s->lhs);
            releaseSlot(); // a call alwas generates a return slot
            break;
        case Statement::If:
            emitIf(s);
            break;
        case Statement::Case:
            emitPlainCase(s);
            break;
        case Statement::TypeCase:
            emitTypeCase(s);
            break;
        case Statement::Loop:
            emitLoop(s);
            break;
        case Statement::While:
            emitWhile(s);
            break;
        case Statement::Repeat:
            emitRepeat(s);
            break;
        case Statement::Exit:
            emitJMP( 0, s->pos.packed() );
            exitJumps << bc.getCurPc();
            break;
        case Statement::Return:
            emitReturn(s->rhs, s->pos);
            break;
        case Statement::ForAssig:
            emitFor(s);
            break;
        }
    }

    void emitImport(Declaration* d)
    {
        Q_ASSERT( d && d->kind == Declaration::Import );
        Import imp = d->data.value<Import>();
        Q_ASSERT(imp.resolved);
        ModuleData md = imp.resolved->data.value<ModuleData>();
        emitImport(md.fullName, d->id, d->pos);
        imports[imp.resolved] = d->id;
    }

    void emitImport( const QByteArray& modName, quint16 toIndex, const RowCol& loc, bool toLocal = false, bool useLuaRequire = false )
    {
        Q_ASSERT( !modName.isEmpty() );
        const int tmp = ctx.back().buySlots(2,true);
        // TEST emitPrint(QString("importing %1 by %2").arg(modName.constData()).arg(thisMod->name.constData()),loc);
        if( useLuaRequire )
            bc.GGET( tmp, "require", loc.packed() ); // cannot use this one for Luon modules
        else
            fetchLnlibMember(tmp, 62, loc); // module.require
        bc.KSET( tmp+1, modName, loc.packed() );
        bc.CALL( tmp, 1, 1, loc.packed() );
        if( toLocal )
            bc.MOV(toIndex,tmp,loc.packed() );
        if( thisMod == ctx.back().scope )
            emitSetTableByIndex(tmp,modSlot,toIndex,loc);
        else
        {
            fetchModule(tmp+1,loc);
            emitSetTableByIndex(tmp,tmp+1,toIndex,loc);
        }
        ctx.back().sellSlots(tmp,2);
    }

    void emitAssureNotNil(quint8 what, const QString& msg, const RowCol& loc)
    {
        const int tmp = ctx.back().buySlots(3, true);
        fetchLnlibMember(tmp, 6, loc); // module.assureNotNil
        bc.MOV( tmp+1, what, loc.packed() );
        bc.KSET(tmp+2, msg, loc.packed());
        bc.CALL( tmp, 0, 2, loc.packed() );
        ctx.back().sellSlots(tmp,3);
    }

    void emitDeferredImports( Declaration* module, const RowCol& loc )
    {
        ModuleData md = module->data.value<ModuleData>();

        Q_ASSERT( module && module->kind == Declaration::Module );
        QMap<Declaration*, quint32>::const_iterator i = imports.find(module);
        Q_ASSERT( i != imports.end() );
        const quint32 index = i.value();

        // at this point require has already been called (recursively) for every module, so
        // we assume the module be available under its full name in the global space
        // if we call require here again instead, we get a circular dependency when instantiating
        // generic modules with meta actuals from the importing module
        const int imported = ctx.back().buySlots(1);
        bc.GGET(imported, md.fullName, loc.packed() );
#ifdef _DEBUG
        emitAssureNotNil(imported, QString("emitImportImplicit module %1 is nil in %2")
                         .arg(md.fullName.constData()).arg(thisMod->name.constData()), loc);
#endif
        if( thisMod == ctx.back().scope )
            emitSetTableByIndex(imported,modSlot,index,loc);
        else
        {
            const int local = ctx.back().buySlots(1);
            fetchModule(local,loc);
            emitSetTableByIndex(imported,local,index,loc);
            ctx.back().sellSlots(local);
        }
        ctx.back().sellSlots(imported);
    }

    quint32 emitImportImplicit( Declaration* module, const RowCol& loc )
    {
        // this is for modules not explicitly on the import list but still used
        // because of e.g. class objects via alias etc.
        // a new slot id is generated in the importing module, which is the only
        // thing required so far.
        Q_ASSERT( module && module->kind == Declaration::Module );
        QMap<Declaration*, quint32>::const_iterator i = imports.find(module);
        if( i != imports.end() )
            return i.value();
        // else
        const quint32 index = thisMod->id++;
        imports[module] = index;

        if( doDeferImports )
        {
            // we have to defer the actual load of the module to the top (i.e. module setup) procedure
            // because only there we can guarantee that the import is run first before the module is used
            // if it is done in sub procedures, the load order and time is arbitrary
            deferred.append(module);
        }else
            emitDeferredImports(module, loc);

        return index;
    }

    void emitCreateRecord( quint8 to, Type* t, const RowCol& loc )
    {
        Type* r = deref(t);
        Q_ASSERT( r );

        const QPair<int, int> counts = t->countAllocRecordMembers(true);
        bc.TNEW( to, counts.first, 0, loc.packed() );

        const int meta = ctx.back().buySlots(1);
        fetchClass(meta, t, loc );

        const int tmp = ctx.back().buySlots(3,true);
        // call setmetatable
        fetchLnlibMember(tmp, 22, loc ); // setmetatable
        bc.MOV(tmp+1, to, loc.packed() );
        bc.MOV(tmp+2, meta, loc.packed() );
        bc.CALL( tmp, 0, 2, loc.packed() );
        ctx.back().sellSlots(tmp,3);
        ctx.back().sellSlots(meta);

        const int val = ctx.back().buySlots(1);
        QList<Declaration*> fields = r->fieldList();
        foreach(Declaration* field, fields )
        {
            if( needsInitializer(field->type) )
            {
                emitInitializer(val, field->type, loc);
                emitSetTableByIndex(val,to, field->id, loc);
            }
        }
        ctx.back().sellSlots(val);
    }

    void emitCreateArray( quint8 to, Type* array, int lenSlot, const RowCol& loc )
    {
        Type* baseType = deref(array->base);
        if( baseType->form == BasicType::CHAR || baseType->form == BasicType::BYTE )
        {
            const int tmp = ctx.back().buySlots(2,true);
            fetchLnlibMember(tmp,8,loc); // module.createCharArray
            if( array->len > 0 )
                bc.KSET(tmp+1, array->len, loc.packed() );
            else if( lenSlot >= 0 )
                bc.MOV(tmp+1, lenSlot, loc.packed() );
            else
                Q_ASSERT(false);
            bc.CALL(tmp,1,1,loc.packed());
            bc.MOV(to,tmp,loc.packed());
            ctx.back().sellSlots(tmp,2);
            // NOTE: len via rt 26 bytesize
        }else
        {
            bc.TNEW( to, array->len, 0, loc.packed() );
            if( array->len > 0 )
            {
                // fix length arrays
                const int tmp = ctx.back().buySlots(1);
                bc.KSET( tmp, array->len, loc.packed() );
                emitSetTableByName(tmp,to,"count",loc);
                if( needsInitializer(array->base) )
                {
                    emitInitializer(tmp, array->base, loc);
                    for( int i = 0; i < array->len; i++ )
                        emitSetTableByIndex(tmp,to, i, loc);
                }
                ctx.back().sellSlots(tmp);
            }else if( lenSlot >= 0 )
            {
                // dynamic length arrays
                const int tmp = ctx.back().buySlots(1);
                emitInitializer(tmp, array->base, loc);
                emitSetTableByName(lenSlot,to,"count",loc);
                if( needsInitializer(array->base) )
                {
                    const quint8 base = ctx.back().buySlots(4);
                    bc.KSET(base,0,loc.packed()); // i := 0
                    bc.SUB(lenSlot, lenSlot, QVariant(1), loc.packed() );
                    bc.MOV(base+1, lenSlot, loc.packed() ); // to len-1
                    bc.KSET(base+2,1,loc.packed()); // by 1
                    bc.FORI(base,0,loc.packed()); // do
                    const quint32 pc = bc.getCurPc();

                    bc.TSET(tmp,to,base+3,loc.packed()); // a[i] := init

                    bc.FORL(base, pc - bc.getCurPc() - 1,loc.packed());
                    bc.patch(pc);
                    ctx.back().sellSlots(base,4);
                    ctx.back().sellSlots(tmp);
                }
            }
        }
    }

    void fetchClass( quint8 to, Type* t, const RowCol& loc )
    {
        Type* record = deref(t);
        Q_ASSERT( record->form == Type::Record );
        // get the module where record was declared and where the class object is
        fetchModule(record->decl->getModule(), to, loc);
        emitGetTableByIndex(to, to, record->decl->id, loc );
    }

    void emitSetTableByIndex( quint8 value, quint8 table, quint32 index, const RowCol& loc)
    {
        if( index > 255 )
        {
            const int tmp = ctx.back().buySlots(1);
            bc.KSET(tmp,index,loc.packed());
            bc.TSET(value,table,tmp,loc.packed());
            ctx.back().sellSlots(tmp);
        }else
            bc.TSETi( value, table, index, loc.packed() );
    }

    void emitSetTableByName( quint8 value, quint8 table, const QByteArray& name, const RowCol& loc)
    {
        if( bc.getConstSlot(name) > 255 )
        {
            const int tmp = ctx.back().buySlots(1);
            bc.KSET(tmp, name, loc.packed());
            bc.TSET(value, table, tmp, loc.packed());
            ctx.back().sellSlots(tmp);
        }else
            bc.TSET(value, table, name, loc.packed());
    }

    void emitGetTableByIndex( quint8 res, quint8 table, quint32 index, const RowCol& loc)
    {
        if( index > 255 )
        {
            const int tmp = ctx.back().buySlots(1);
            bc.KSET(tmp,index,loc.packed());
            bc.TGET(res,table,tmp,loc.packed());
            ctx.back().sellSlots(tmp);
        }else
            bc.TGETi( res, table, index, loc.packed() );
    }

    void fetchLnlib( quint8 to, const RowCol& loc )
    {
        if( ctx.back().scope == thisMod )
            bc.MOV( to, lnlj, loc.packed() );
        else
            bc.UGET(to, ctx.back().resolveUpval(lnlj,"@lnlj"), loc.packed() );
    }

    void fetchLnlibMember( quint8 to, quint8 what, const RowCol& loc )
    {
        if( ctx.back().scope == thisMod )
            bc.TGETi(to,lnlj,what,loc.packed());
        else
        {
            fetchLnlib(to,loc);
            bc.TGETi(to,to,what,loc.packed());
        }
    }

    void fetchModule( Declaration* module, quint8 to, const RowCol& loc )
    {
        if( module == thisMod )
            return fetchModule(to,loc);
        // else
        const quint32 index = emitImportImplicit(module, loc);
        fetchModule(to, loc); // get this module where the import slots live
        emitGetTableByIndex(to, to, index, loc);
    }

    void fetchModule( quint8 to, const RowCol& loc )
    {
        if( ctx.back().scope == thisMod )
            bc.MOV( to, modSlot, loc.packed() );
        else
            bc.UGET(to, ctx.back().resolveUpval(modSlot,"@mod"), loc.packed() );
    }

    Type* deref(Type* t) const
    {
        // never returns zero
        if( t == 0 )
            return mdl.getType(BasicType::NoType);
        return t->deref();
    }
};

LjbcGen::LjbcGen()
{
    imp = new Imp();
}

LjbcGen::~LjbcGen()
{
    delete imp;
}

bool LjbcGen::translate(Declaration* module, QIODevice* out, bool strip)
{
    try
    {
        return imp->visitModule(module,out,strip);
    }catch(...)
    {
        return false;
    }
}

static void allocateLocalSlots(quint32& modSlotNr, Declaration* cur);

static void allocateClassSlots(quint32& modSlotNr, Declaration* cur)
{
    // slot per class object
    cur->id = modSlotNr++;

    if( cur->type )
    {
        Type* record = cur->type->deref();
        record->countAllocRecordMembers(true);
        foreach( Declaration* sub, record->subs)
        {
            if( sub->kind == Declaration::Procedure )
                allocateLocalSlots(modSlotNr, sub);
        }
    }
}

static void allocateLocalSlots(quint32& modSlotNr, Declaration* cur)
{
    Declaration* d = cur->link;
    quint32 localSlot = 0;
    while( d )
    {
        switch( d->kind )
        {
        case Declaration::TypeDecl:
            if( d->type && d->type->form == Type::Record )
                allocateClassSlots(modSlotNr, d);
            break;
        case Declaration::ConstDecl:
            // no slot
            break;
        case Declaration::Procedure:
            d->id = modSlotNr++;
            allocateLocalSlots(modSlotNr, d);
            break;
        case Declaration::LocalDecl:
            d->id = localSlot++;
            break;
        case Declaration::ParamDecl:
            d->id = localSlot++;
            break;
        }

        d = d->getNext();
    }
}

bool LjbcGen::allocateModuleSlots(Declaration* module)
{
    Q_ASSERT( module && module->kind == Declaration::Module );

    quint32 modSlotNr = 2; // spare the first two slots for LUON internals etc.

    Declaration* d = module->link;
    while( d )
    {
        switch( d->kind )
        {
        case Declaration::TypeDecl:
            if( d->type && d->type->form == Type::Record )
                allocateClassSlots(modSlotNr, d);
            break;
        case Declaration::ConstDecl:
        case Declaration::VarDecl:
        case Declaration::Import:
            d->id = modSlotNr++;
            break;
        case Declaration::Procedure:
            d->id = modSlotNr++;
            allocateLocalSlots(modSlotNr, d);
            break;
        }

        d = d->getNext();
    }
    module->id = modSlotNr;
}

const QList<LjbcGen::Error>&LjbcGen::getErrors() const
{
    return imp->errors;
}

