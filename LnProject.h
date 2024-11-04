#ifndef OBXPROJECT_H
#define OBXPROJECT_H

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

// Adopted from the Oberon+ IDE

#include <QObject>
#include <QStringList>
#include <QExplicitlySharedDataPointer>
#include <Luon/LnValidator.h>

class QDir;
class QBuffer;

namespace Ln
{
    class Project : public QObject, public Importer
    {
#ifndef QT_NO_QOBJECT
        Q_OBJECT
#endif
    public:
        struct Error {
            QString msg;
            RowCol pos;
            QString path;
            Error( const QString& m, const RowCol& pos, const QString& p):msg(m),pos(pos),path(p){}
        };

        struct FileGroup;
        struct File : public QSharedData
        {
            QString d_filePath;
            QByteArray d_name;
            QByteArray d_cache;
            FileGroup* d_group;
            Declaration* d_mod;
            bool d_isLib;
            File():d_isLib(false),d_group(0),d_mod(0){}
        };
        typedef QExplicitlySharedDataPointer<File> FileRef;

        struct FileGroup
        {
            QByteArrayList d_package;
            QList<File*> d_files;
        };

        typedef QList<FileGroup> FileGroups;
        typedef QHash<QString,FileRef> FileHash; // FilePath -> File
        typedef QList<FileRef> FileList;
        typedef QPair<QByteArray,QByteArray> ModProc; // module.procedure or just module

        explicit Project(QObject *parent = 0);
        ~Project();

        void clear();

        void createNew();
        bool initializeFromDir( const QDir&, bool recursive = false );
        bool loadFrom( const QString& filePath );
        bool save();
        bool saveTo(const QString& filePath );
        void setSuffixes( const QStringList& ); // Form: ".suffix"
        const QStringList& getSuffixes() const { return d_suffixes; }
        const QString& getProjectPath() const { return d_filePath; }
        bool isDirty() const { return d_dirty; }

        void setMain( const ModProc& );
        const ModProc& getMain() const { return d_main; }
        QString renderMain() const;
        void setUseBuiltInOakwood(bool);
        bool useBuiltInOakwood() const { return d_useBuiltInOakwood; }
        void setUseBuiltInObSysInner(bool);
        bool useBuiltInObSysInner() const { return d_useBuiltInObSysInner; }
        QString getWorkingDir(bool resolved = false) const;
        void setWorkingDir( const QString& );
        QString getBuildDir(bool resolved = false) const;
        void setBuildDir( const QString& );
        QByteArrayList getOptions() const { return d_options; }
        void setOptions( const QByteArrayList& );

        bool addFile(const QString& filePath, const QByteArrayList& package = QByteArrayList() );
        bool removeFile( const QString& filePath );
        bool addPackagePath(const QByteArrayList& path );
        bool removePackagePath( const QByteArrayList& path );

        bool parse();

        const FileHash& getFiles() const { return d_files; }
        const FileGroups& getFileGroups() const { return d_groups; }
        const FileGroup* getRootFileGroup() const;
        const FileGroup* findFileGroup(const QByteArrayList& package ) const;
        File* findFile( const QString& file ) const;
        void addPreload( const QByteArray& name, const QByteArray& code );

        Symbol* findSymbolBySourcePos(const QString& file, quint32 line, quint16 col, Declaration** = 0 ) const;
        Symbol* findSymbolBySourcePos(Declaration*, quint32 line, quint16 col, Declaration** scopePtr) const;
        typedef QList<QPair<Declaration*, SymList> > UsageByMod;
        UsageByMod getUsage( Declaration* ) const;
        Symbol* getSymbolsOfModule(Declaration*) const;
        DeclList getSubs(Declaration*) const;
        DeclList getModulesToGenerate() const;

        bool printTreeShaken( const QString& module, const QString& fileName );
        bool printImportDependencies(const QString& fileName , bool pruned);

        const QList<Error>& getErrors() const {return errors; }
        void addError(const QString& file, const RowCol& pos, const QString& msg);
    signals:
        void sigModified(bool);
        void sigRenamed();
        void sigReparsed();
    protected:
        QStringList findFiles(const QDir& , bool recursive = false);
        void touch();
        int findPackage(const QByteArrayList& path ) const;

        QByteArray modulePath( const QByteArrayList& path );
        QByteArray moduleSuffix( const MetaActualList& ma );
        Declaration* loadModule( const Import& imp );

        struct ModuleSlot
        {
            Import imp;
            QString file;
            Declaration* decl;
            Xref xref;
            ModuleSlot():decl(0) {}
            ModuleSlot( const Import& i, const QString& f, Declaration* d):imp(i),file(f),decl(d){}
        };
        ModuleSlot* find(const Import& imp);
        File* toFile(const Import& imp);
        void clearModules();
        const ModuleSlot* findModule(Declaration*) const;
    private:
        typedef QList<ModuleSlot> Modules;
        Modules modules;
        QHash<Declaration*, DeclList> subs;
        FileList preloads;
        QList<Error> errors;
        FileHash d_files;
        FileGroups d_groups;
        QString d_filePath; // path where the project file was loaded from or saved to
        QStringList d_suffixes;
        QByteArrayList d_options;
        QString d_workingDir, d_buildDir;
        ModProc d_main;
        bool d_dirty;
        bool d_useBuiltInOakwood;
        bool d_useBuiltInObSysInner;
    };
}

#endif // OBXPROJECT_H
