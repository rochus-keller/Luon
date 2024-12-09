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

#include "LnIde.h"
#include "LnHighlighter.h"
#include "LnAst.h"
#include "LnLjRuntime.h"
#include "LnLexer.h"
#include "LnProject.h"
#include <LjTools/Engine2.h>
#include <LjTools/Terminal2.h>
#include <LjTools/BcViewer2.h>
#include <LjTools/BcViewer.h>
#include <LjTools/LuaJitEngine.h>
#include <LjTools/LjBcDebugger.h>
#include <QtDebug>
#include <QDockWidget>
#include <QApplication>
#include <QStandardPaths>
#include <QDir>
#include <QDateTime>
#include <QSettings>
#include <QShortcut>
#include <QScrollBar>
#include <QMessageBox>
#include <QFileDialog>
#include <QFileInfo>
#include <QBuffer>
#include <QHeaderView>
#include <QLabel>
#include <QVBoxLayout>
#include <QDesktopWidget>
#include <QInputDialog>
#include <GuiTools/AutoMenu.h>
#include <GuiTools/CodeEditor.h>
#include <GuiTools/AutoShortcut.h>
#include <GuiTools/DocTabWidget.h>
#include <lua.hpp>
using namespace Ln;

#ifdef Q_OS_MAC
#define OBN_BREAK_SC "SHIFT+F8"
#define OBN_ABORT_SC "CTRL+SHIFT+Y"
#define OBN_CONTINUE_SC "CTRL+Y"
#define OBN_STEPIN_SC "CTRL+SHIFT+I"
#define OBN_STEPOVER_SC "CTRL+SHIFT+O"
#define OBN_STEPOUT_SC "SHIFT+F11" // TODO
#define OBN_ENDBG_SC "F4"
#define OBN_TOGBP_SC "F8"
#define OBN_GOBACK_SC "ALT+CTRL+Left"
#define OBN_GOFWD_SC "ALT+CTRL+Right"
#define OBN_NEXTDOC_SC "ALT+TAB"
#define OBN_PREVDOC_SC "ALT+SHIFT+TAB"
#else
#define OBN_BREAK_SC "SHIFT+F9"
#define OBN_ABORT_SC "SHIFT+F5"
#define OBN_CONTINUE_SC "F5"
#define OBN_STEPIN_SC "F11"
#define OBN_STEPOVER_SC "F10"
#define OBN_STEPOUT_SC "SHIFT+F11"
#define OBN_ENDBG_SC "F8"
#define OBN_TOGBP_SC "F9"
#define OBN_GOBACK_SC "ALT+Left"
#define OBN_GOFWD_SC "ALT+Right"
#define OBN_NEXTDOC_SC "CTRL+TAB"
#define OBN_PREVDOC_SC "CTRL+SHIFT+TAB"
#endif

class Ide::Editor : public CodeEditor
{
public:
    Editor(Ide* p, Project* pro):CodeEditor(p),d_pro(pro),d_ide(p),dbgRow(0),dbgCol(0)
    {
        setCharPerTab(2);
        setTypingLatency(400);
        setPaintIndents(false);
        d_hl = new Highlighter( document() );
        updateTabWidth();
        QSettings set;
        if( !set.contains("CodeEditor/Font") )
        {
            QFont monospace("Monospace",9);
            if( !monospace.exactMatch() )
            {
                monospace = QFont("DejaVu Sans Mono",9);
                monospace.setStyleName("Book");
            }
            setFont(monospace);
        }
    }

    ~Editor()
    {
    }

    Ide* d_ide;
    Highlighter* d_hl;
    Project* d_pro;
    int dbgRow, dbgCol;

    void setExt( bool on )
    {
        for( int i = Builtin::ABS; i < Builtin::Max; i++ )
            d_hl->addBuiltIn(Builtin::name[i]);
        for( int i = BasicType::Nil; i <= BasicType::Max; i++ )
            d_hl->addBuiltIn(BasicType::name[i]);
        d_hl->addBuiltIn("ANYREC");
    }

    void clearBackHisto()
    {
        d_backHisto.clear();
    }

    void markNonTerms(const SymList& syms)
    {
        d_nonTerms.clear();
        QTextCharFormat format;
        format.setBackground( QColor(237,235,243) );
        foreach( Symbol* s, syms )
        {
            Declaration* ident = s->decl;
            Q_ASSERT( ident );
            QTextCursor c( document()->findBlockByNumber( qMax(int(s->pos.d_row - 1),0)) );
            c.setPosition( c.position() + qMax(int(s->pos.d_col - 1), 0) );
            int pos = c.position();
            c.setPosition( pos + ident->name.size(), QTextCursor::KeepAnchor );

            QTextEdit::ExtraSelection sel;
            sel.format = format;
            sel.cursor = c;

            d_nonTerms << sel;
        }
        updateExtraSelections();
    }

    void updateExtraSelections()
    {
        ESL sum;

        QTextEdit::ExtraSelection line;
        line.format.setBackground(QColor(Qt::yellow).lighter(170));
        line.format.setProperty(QTextFormat::FullWidthSelection, true);
        line.cursor = textCursor();
        line.cursor.clearSelection();
        sum << line;

        sum << d_nonTerms;

        if( !d_pro->getErrors().isEmpty() )
        {
            QTextCharFormat errorFormat;
            errorFormat.setUnderlineStyle(QTextCharFormat::WaveUnderline);
            errorFormat.setUnderlineColor(Qt::magenta);
            for( int i = 0; i < d_pro->getErrors().size(); i++ )
            {
                const Project::Error& e = d_pro->getErrors().at(i);
                if( e.path != getPath() )
                    continue;
                QTextCursor c( document()->findBlockByNumber(e.pos.d_row - 1) );

                c.setPosition( c.position() + e.pos.d_col - 1 );
                c.movePosition(QTextCursor::EndOfWord, QTextCursor::KeepAnchor);

                QTextEdit::ExtraSelection sel;
                sel.format = errorFormat;
                sel.cursor = c;
                sel.format.setToolTip(e.msg);

                sum << sel;
            }
        }

        sum << d_link;

        if( d_ide->d_rt->getLua()->isDebug() && d_ide->d_rt->getLua()->isExecuting() &&
                ( d_ide->d_rt->getLua()->getMode() == Lua::Engine2::RowColMode ||
                  d_ide->d_rt->getLua()->getMode() == Lua::Engine2::PcMode ) )
        {
            if( d_ide->d_rt->getLua()->getMode() == Lua::Engine2::RowColMode )
            {
                dbgRow = RowCol::unpackRow(d_ide->d_rt->getLua()->getCurRowCol())-1;
                dbgCol = RowCol::unpackCol(d_ide->d_rt->getLua()->getCurRowCol())-1;
            }
            QTextEdit::ExtraSelection line;
            line.format.setBackground(QColor(Qt::yellow));
            line.format.setUnderlineStyle(QTextCharFormat::SingleUnderline);
            line.format.setUnderlineColor(Qt::red);
            line.cursor = QTextCursor(document()->findBlockByNumber(dbgRow));
            line.cursor.setPosition(line.cursor.position() + dbgCol);
            line.cursor.select(QTextCursor::WordUnderCursor);
            sum << line;
        }

        setExtraSelections(sum);
    }

    void mousePressEvent(QMouseEvent* e)
    {
        if( !d_link.isEmpty() )
        {
            QTextCursor cur = cursorForPosition(e->pos());
            d_ide->pushLocation( Ide::Location( getPath(), cur.blockNumber(), cur.positionInBlock(), verticalScrollBar()->value() ) );
            QApplication::restoreOverrideCursor();
            d_link.clear();
        }
        if( QApplication::keyboardModifiers() == Qt::ControlModifier )
        {
            QTextCursor cur = cursorForPosition(e->pos());
            Symbol* e = d_pro->findSymbolBySourcePos(
                        getPath(),cur.blockNumber() + 1,cur.positionInBlock() + 1);
            if( e )
            {
                Declaration* decl = e->decl;
                d_ide->pushLocation( Ide::Location( getPath(), cur.blockNumber(), cur.positionInBlock(), verticalScrollBar()->value() ) );
                if( decl->kind == Declaration::Import && e->pos == decl->pos )
                {
                    Import imp = decl->data.value<Import>();
                    decl = imp.resolved;
                }else if( decl->kind == Declaration::Procedure )
                {
                    if( decl->mode == Declaration::Receiver && decl->super )
                        decl = decl->super;
                }
                d_ide->showEditor( decl, false, true );
                //setCursorPosition( sym->pos.d_row - 1, sym->pos.d_col - 1, true );
            }
            updateExtraSelections();
        }else
            QPlainTextEdit::mousePressEvent(e);
    }

    void mouseMoveEvent(QMouseEvent* e)
    {
        QPlainTextEdit::mouseMoveEvent(e);
        if( QApplication::keyboardModifiers() == Qt::ControlModifier )
        {
            QTextCursor cur = cursorForPosition(e->pos());
            Symbol* e = d_pro->findSymbolBySourcePos(
                        getPath(),cur.blockNumber() + 1,cur.positionInBlock() + 1);
            const bool alreadyArrow = !d_link.isEmpty();
            d_link.clear();
            if( e )
            {
                Declaration* sym = e->decl;
                Q_ASSERT(sym);
                const int off = cur.positionInBlock() + 1 - e->pos.d_col;
                cur.setPosition(cur.position() - off);
                cur.setPosition( cur.position() + sym->name.size(), QTextCursor::KeepAnchor );

                QTextEdit::ExtraSelection sel;
                sel.cursor = cur;
                sel.format.setFontUnderline(true);
                d_link << sel;
                /*
                d_linkLineNr = sym->pos.d_row - 1;
                d_linkColNr = sym->pos.d_col - 1;
                */
                if( !alreadyArrow )
                    QApplication::setOverrideCursor(Qt::ArrowCursor);
            }
            if( alreadyArrow && d_link.isEmpty() )
                QApplication::restoreOverrideCursor();
            updateExtraSelections();
        }else if( !d_link.isEmpty() )
        {
            QApplication::restoreOverrideCursor();
            d_link.clear();
            updateExtraSelections();
        }
    }

    void onUpdateModel()
    {
        markNonTerms(SymList());
    }
};

class Ide::DocTab : public DocTabWidget
{
public:
    DocTab(QWidget* p):DocTabWidget(p,false) {}

    // overrides
    bool isUnsaved(int i)
    {
        Ide::Editor* edit = static_cast<Ide::Editor*>( widget(i) );
        return edit->isModified();
    }

    bool save(int i)
    {
        Ide::Editor* edit = static_cast<Ide::Editor*>( widget(i) );
        if( !edit->saveToFile( edit->getPath(), false ) )
            return false;
        return true;
    }
};

class Ide::Debugger : public Lua::DbgShell
{
public:
    Ide* d_ide;
    Debugger(Ide* ide):d_ide(ide){}
    void handleBreak( Lua::Engine2* lua, const QByteArray& source, quint32 line )
    {
        QByteArray msg = lua->getValueString(1).simplified();

        d_ide->enableDbgMenu();
        d_ide->fillStack();
        d_ide->fillLocals();

        if( msg.startsWith('"') )
            msg = msg.mid(1,msg.size()-2); // remove ""
        if( !lua->isBreakHit() )
        {
            if( source == "=[C]" && !msg.startsWith('[') )
                msg = "[\"=[C]\"]:" + QByteArray::number(RowCol::unpackRow(line)) + ":" + msg;
            if( d_ide->luaRuntimeMessage(msg,source) )
                d_ide->onErrors();
        }

        while( lua->isWaiting() )
        {
            QApplication::processEvents(QEventLoop::AllEvents | QEventLoop::WaitForMoreEvents );
            QApplication::flush();
        }
        d_ide->removePosMarkers();
        d_ide->enableDbgMenu();
        d_ide->d_stack->clear();
        d_ide->d_locals->clear();
    }
    void handleAliveSignal(Lua::Engine2* e)
    {
        QApplication::processEvents(QEventLoop::AllEvents | QEventLoop::WaitForMoreEvents );
        QApplication::flush();
    }
};


static Ide* s_this = 0;
static void report(QtMsgType type, const QString& message )
{
    if( s_this )
    {
        switch(type)
        {
        case QtDebugMsg:
            // NOP s_this->logMessage(QLatin1String("INF: ") + message);
            break;
        case QtWarningMsg:
            s_this->logMessage(QLatin1String("WRN: ") + message);
            break;
        case QtCriticalMsg:
        case QtFatalMsg:
            s_this->logMessage(QLatin1String("ERR: ") + message, true);
            break;
        }
    }
}
static QtMessageHandler s_oldHandler = 0;
void messageHander(QtMsgType type, const QMessageLogContext& ctx, const QString& message)
{
    if( s_oldHandler )
        s_oldHandler(type, ctx, message );
    report(type,message);
}

static void log( const QString& msg )
{
    if( s_this )
        s_this->logMessage(msg);
}

Ide::Ide(QWidget *parent)
    : QMainWindow(parent),d_lock(false),d_filesDirty(false),d_pushBackLock(false),
      d_lock2(false),d_lock3(false),d_lock4(false),d_curModule(0)
{
    s_this = this;
    // TODO LibFfi::setSendToLog(log);

    d_rt = new LjRuntime(this);

    d_dbg = new Debugger(this);
    d_rt->getLua()->setDbgShell(d_dbg);
    // d_rt->getLua()->setAliveSignal(true); // reduces performance by factor 2 to 5
    connect( d_rt->getLua(), SIGNAL(onNotify(int,QByteArray,int)),this,SLOT(onLuaNotify(int,QByteArray,int)) );

    d_tab = new DocTab(this);
    d_tab->setCloserIcon( ":/images/close.png" );
    Gui::AutoMenu* pop = new Gui::AutoMenu( d_tab, true );
    pop->addCommand( tr("Forward Tab"), d_tab, SLOT(onDocSelect()), tr(OBN_NEXTDOC_SC) );
    pop->addCommand( tr("Backward Tab"), d_tab, SLOT(onDocSelect()), tr(OBN_PREVDOC_SC) );
    pop->addCommand( tr("Close Tab"), d_tab, SLOT(onCloseDoc()), tr("CTRL+W") );
    pop->addCommand( tr("Close All"), d_tab, SLOT(onCloseAll()) );
    pop->addCommand( tr("Close All Others"), d_tab, SLOT(onCloseAllButThis()) );
    addTopCommands( pop );

    new Gui::AutoShortcut( tr(OBN_NEXTDOC_SC), this, d_tab, SLOT(onDocSelect()) );
    new Gui::AutoShortcut( tr(OBN_PREVDOC_SC), this, d_tab, SLOT(onDocSelect()) );
    new Gui::AutoShortcut( tr("CTRL+W"), this, d_tab, SLOT(onCloseDoc()) );

    connect( d_tab, SIGNAL( currentChanged(int) ), this, SLOT(onTabChanged() ) );
    connect( d_tab, SIGNAL(closing(int)), this, SLOT(onTabClosing(int)) );

    setDockNestingEnabled(true);
    setCorner( Qt::BottomRightCorner, Qt::RightDockWidgetArea );
    setCorner( Qt::BottomLeftCorner, Qt::LeftDockWidgetArea );
    setCorner( Qt::TopRightCorner, Qt::RightDockWidgetArea );
    setCorner( Qt::TopLeftCorner, Qt::LeftDockWidgetArea );

    d_dbgBreak = new QAction(tr("Break"),this);
    d_dbgBreak->setShortcutContext(Qt::ApplicationShortcut);
    d_dbgBreak->setShortcut(tr(OBN_BREAK_SC));
    addAction(d_dbgBreak);
    connect( d_dbgBreak, SIGNAL(triggered(bool)),this,SLOT(onBreak()) );
    d_dbgAbort = new QAction(tr("Abort"),this);
    d_dbgAbort->setShortcutContext(Qt::ApplicationShortcut);
    d_dbgAbort->setShortcut(tr(OBN_ABORT_SC));
    addAction(d_dbgAbort);
    connect( d_dbgAbort, SIGNAL(triggered(bool)),this,SLOT(onAbort()) );
    d_dbgContinue = new QAction(tr("Continue"),this);
    d_dbgContinue->setShortcutContext(Qt::ApplicationShortcut);
    d_dbgContinue->setShortcut(tr(OBN_CONTINUE_SC));
    addAction(d_dbgContinue);
    connect( d_dbgContinue, SIGNAL(triggered(bool)),this,SLOT(onContinue()) );
    d_dbgStepIn = new QAction(tr("Step In"),this);
    d_dbgStepIn->setShortcutContext(Qt::ApplicationShortcut);
    d_dbgStepIn->setShortcut(tr(OBN_STEPIN_SC));
    addAction(d_dbgStepIn);
    connect( d_dbgStepIn, SIGNAL(triggered(bool)),this,SLOT(onSingleStep()) );
    d_dbgStepOver = new QAction(tr("Step Over"),this);
    d_dbgStepOver->setShortcutContext(Qt::ApplicationShortcut);
    d_dbgStepOver->setShortcut(tr(OBN_STEPOVER_SC));
    addAction(d_dbgStepOver);
    connect( d_dbgStepOver, SIGNAL(triggered(bool)),this,SLOT(onStepOver()) );
    d_dbgStepOut = new QAction(tr("Step Out"),this);
    d_dbgStepOut->setShortcutContext(Qt::ApplicationShortcut);
    d_dbgStepOut->setShortcut(tr(OBN_STEPOUT_SC));
    addAction(d_dbgStepOut);
    connect( d_dbgStepOut, SIGNAL(triggered(bool)),this,SLOT(onStepOut()) );

    enableDbgMenu();

    createMods();
    createMod();
    createHier();
    createXref();
    createErrs();
    createDumpView();
    createLocals();
    createStack();
    createTerminal();
    createMenu();

    setCentralWidget(d_tab);

    createMenuBar();

    s_oldHandler = qInstallMessageHandler(messageHander);

    QSettings s;

    const QRect screen = QApplication::desktop()->screenGeometry();
    resize( screen.width() - 20, screen.height() - 30 ); // so that restoreState works
    if( s.value("Fullscreen").toBool() )
        showFullScreen();
    else
        showMaximized();

    const QVariant state = s.value( "DockState" );
    if( !state.isNull() )
        restoreState( state.toByteArray() );


    connect( d_rt->getPro(),SIGNAL(sigRenamed()),this,SLOT(onCaption()) );
    connect( d_rt->getPro(),SIGNAL(sigModified(bool)),this,SLOT(onCaption()) );
}

Ide::~Ide()
{
    delete d_dbg;
}

void Ide::loadFile(const QString& path)
{
    QFileInfo info(path);

    if( info.isDir() && info.suffix() != ".lnpro" )
    {
        d_rt->getPro()->initializeFromDir( path );
    }else
    {
        d_rt->getPro()->loadFrom(path);
    }

    QDir::setCurrent(QFileInfo(path).absolutePath());

    onCaption();

    onCompile();
}

void Ide::logMessage(const QString& str, bool err)
{
    d_term->printText(str,err);
}

void Ide::closeEvent(QCloseEvent* event)
{
    QSettings s;
    s.setValue( "DockState", saveState() );
    const bool ok = checkSaved( tr("Quit Application"));
    event->setAccepted(ok);
    if( ok )
    {
        d_rt->getLua()->terminate(true);
        // TODO SysInnerLib::quit();
    }
}

void Ide::createTerminal()
{
    QDockWidget* dock = new QDockWidget( tr("Terminal"), this );
    dock->setObjectName("Terminal");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    d_term = new Lua::Terminal2(dock, d_rt->getLua());
    dock->setWidget(d_term);
    addDockWidget( Qt::BottomDockWidgetArea, dock );
    new Gui::AutoShortcut( tr("CTRL+SHIFT+C"), this, d_term, SLOT(onClear()) );
    connect( d_rt->getLua(), SIGNAL(onNotify(int,QByteArray,int)), dock, SLOT(show()) );
}

void Ide::createDumpView()
{
    QDockWidget* dock = new QDockWidget( tr("Bytecode"), this );
    dock->setVisible(false);
    dock->setObjectName("Bytecode");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    d_bcv = new Lua::BcViewer2(dock);
    dock->setWidget(d_bcv);
    addDockWidget( Qt::RightDockWidgetArea, dock );
    connect(d_bcv,SIGNAL(sigGotoLine(quint32)),this,SLOT(onGotoLnr(quint32)));

    Gui::AutoMenu* pop = new Gui::AutoMenu( d_bcv, true );
    pop->addCommand( "Run on LuaJIT", this, SLOT(onRun()), tr("CTRL+R"), false );
    addDebugMenu(pop);
    pop->addSeparator();
    pop->addCommand( "Show low level bytecode", this, SLOT(onShowLlBc()) );
    pop->addCommand( "Export current bytecode...", this, SLOT(onExportBc()) );
    pop->addCommand( "Show bytecode file...", this, SLOT(onShowBcFile()) );
    addTopCommands(pop);
}

void Ide::createMods()
{
    QDockWidget* dock = new QDockWidget( tr("Modules"), this );
    dock->setObjectName("Modules");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable );
    d_mods = new QTreeWidget(dock);
    d_mods->setHeaderHidden(true);
    d_mods->setExpandsOnDoubleClick(false);
    d_mods->setAlternatingRowColors(true);
    dock->setWidget(d_mods);
    addDockWidget( Qt::LeftDockWidgetArea, dock );
    connect( d_mods, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)),this,SLOT(onModsDblClicked(QTreeWidgetItem*,int)) );
}

void Ide::createMod()
{
    QDockWidget* dock = new QDockWidget( tr("Module"), this );
    dock->setObjectName("Module");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    QWidget* pane = new QWidget(dock);
    QVBoxLayout* vbox = new QVBoxLayout(pane);
    vbox->setMargin(0);
    vbox->setSpacing(0);
    d_modTitle = new QLabel(pane);
    d_modTitle->setMargin(2);
    d_modTitle->setWordWrap(true);
    vbox->addWidget(d_modTitle);
    d_mod = new QTreeWidget(dock);
    d_mod->setHeaderHidden(true);
    d_mod->setExpandsOnDoubleClick(false);
    d_mod->setAlternatingRowColors(true);
    vbox->addWidget(d_mod);
    dock->setWidget(pane);
    addDockWidget( Qt::LeftDockWidgetArea, dock );
    connect( d_mod, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)),this,SLOT(onModDblClicked(QTreeWidgetItem*,int)) );
}

void Ide::createHier()
{
    QDockWidget* dock = new QDockWidget( tr("Hierarchy"), this );
    dock->setObjectName("Hierarchy");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    QWidget* pane = new QWidget(dock);
    QVBoxLayout* vbox = new QVBoxLayout(pane);
    vbox->setMargin(0);
    vbox->setSpacing(0);
    d_hierTitle = new QLabel(pane);
    d_hierTitle->setMargin(2);
    d_hierTitle->setWordWrap(true);
    vbox->addWidget(d_hierTitle);
    d_hier = new QTreeWidget(dock);
    d_hier->setHeaderHidden(true);
    d_hier->setExpandsOnDoubleClick(false);
    d_hier->setAlternatingRowColors(true);
    vbox->addWidget(d_hier);
    dock->setWidget(pane);
    addDockWidget( Qt::LeftDockWidgetArea, dock );
    connect( d_hier, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)),this,SLOT(onHierDblClicked(QTreeWidgetItem*,int)) );
}

void Ide::createErrs()
{
    QDockWidget* dock = new QDockWidget( tr("Issues"), this );
    dock->setObjectName("Issues");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    d_errs = new QTreeWidget(dock);
    d_errs->setSizePolicy(QSizePolicy::MinimumExpanding,QSizePolicy::Preferred);
    d_errs->setAlternatingRowColors(true);
    d_errs->setHeaderHidden(true);
    d_errs->setSortingEnabled(false);
    d_errs->setAllColumnsShowFocus(true);
    d_errs->setRootIsDecorated(false);
    d_errs->setColumnCount(3);
    d_errs->header()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    d_errs->header()->setSectionResizeMode(1, QHeaderView::ResizeToContents);
    d_errs->header()->setSectionResizeMode(2, QHeaderView::Stretch);
    dock->setWidget(d_errs);
    addDockWidget( Qt::BottomDockWidgetArea, dock );
    connect(d_errs, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)), this, SLOT(onErrorsDblClicked()) );
    connect( new QShortcut( tr("ESC"), this ), SIGNAL(activated()), dock, SLOT(hide()) );
}

void Ide::createXref()
{
    QDockWidget* dock = new QDockWidget( tr("Xref"), this );
    dock->setObjectName("Xref");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    QWidget* pane = new QWidget(dock);
    QVBoxLayout* vbox = new QVBoxLayout(pane);
    vbox->setMargin(0);
    vbox->setSpacing(0);
    d_xrefTitle = new QLabel(pane);
    d_xrefTitle->setMargin(2);
    d_xrefTitle->setWordWrap(true);
    vbox->addWidget(d_xrefTitle);
    d_xref = new QTreeWidget(pane);
    d_xref->setAlternatingRowColors(true);
    d_xref->setHeaderHidden(true);
    d_xref->setAllColumnsShowFocus(true);
    d_xref->setRootIsDecorated(false);
    vbox->addWidget(d_xref);
    dock->setWidget(pane);
    addDockWidget( Qt::LeftDockWidgetArea, dock );
    connect(d_xref, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)), this, SLOT(onXrefDblClicked()) );
}

void Ide::createStack()
{
    QDockWidget* dock = new QDockWidget( tr("Stack"), this );
    dock->setVisible(false);
    dock->setObjectName("Stack");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    d_stack = new QTreeWidget(dock);
    d_stack->setHeaderHidden(true);
    d_stack->setAlternatingRowColors(true);
    d_stack->setColumnCount(4); // Level, Name, Pos, Mod
    d_stack->header()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    d_stack->header()->setSectionResizeMode(1, QHeaderView::ResizeToContents);
    d_stack->header()->setSectionResizeMode(2, QHeaderView::ResizeToContents);
    d_stack->header()->setSectionResizeMode(3, QHeaderView::Stretch);
    dock->setWidget(d_stack);
    addDockWidget( Qt::RightDockWidgetArea, dock );
    connect( d_stack, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)),this,SLOT(onStackDblClicked(QTreeWidgetItem*,int)) );
}

void Ide::createLocals()
{
    QDockWidget* dock = new QDockWidget( tr("Locals"), this );
    dock->setVisible(false);
    dock->setObjectName("Locals");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    d_locals = new QTreeWidget(dock);
    d_locals->setHeaderHidden(true);
    d_locals->setAlternatingRowColors(true);
    d_locals->setColumnCount(2); // Name, Value
    d_locals->header()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    d_locals->header()->setSectionResizeMode(1, QHeaderView::Stretch);
    dock->setWidget(d_locals);
    addDockWidget( Qt::RightDockWidgetArea, dock );
}

void Ide::createMenu()
{
    Gui::AutoMenu* pop = new Gui::AutoMenu( d_mods, true );
    pop->addCommand( "Show File", this, SLOT(onOpenFile()) );
    pop->addAction("Expand all", d_mods, SLOT(expandAll()) );
    pop->addSeparator();
    pop->addCommand( "New Project", this, SLOT(onNewPro()), tr("CTRL+N"), false );
    pop->addCommand( "Open Project...", this, SLOT(onOpenPro()), tr("CTRL+O"), false );
    pop->addCommand( "Save Project", this, SLOT(onSavePro()), tr("CTRL+SHIFT+S"), false );
    pop->addCommand( "Save Project as...", this, SLOT(onSaveAs()) );
    pop->addSeparator();
    pop->addCommand( "New Module...", this, SLOT(onNewModule()), tr("CTRL+SHIFT+N"), false );
    pop->addCommand( "Add existing Modules...", this, SLOT(onAddFiles()) );
    pop->addCommand( "Remove Module...", this, SLOT(onRemoveFile()) );
    pop->addCommand( "Export minimized Module...", this, SLOT(onExpMod()) );
    pop->addSeparator();
    pop->addCommand( "Add Import Path...", this, SLOT(onAddDir()) );
    pop->addCommand( "Remove Import Path...", this, SLOT(onRemoveDir()) );
    pop->addSeparator();
    pop->addCommand( "Built-in Oakwood", this, SLOT(onOakwood()) );
    pop->addCommand( "Set Configuration Variables...", this, SLOT( onSetOptions()) );
    pop->addCommand( "File System Root...", this, SLOT( onWorkingDir() ) );
    pop->addSeparator();
    pop->addCommand( "Compile", this, SLOT(onCompile()), tr("CTRL+T"), false );
    pop->addCommand( "Compile && Generate", this, SLOT(onGenerate()), tr("CTRL+SHIFT+T"), false );
    pop->addCommand( "JIT Enabled", this, SLOT(onJitEnabled()) );
    pop->addCommand( "Restart LuaJIT", this, SLOT(onRestartLua()), tr("CTRL+SHIFT+R"), false );
    pop->addCommand( "Set Command...", this, SLOT(onRunCommand()) );
    pop->addCommand( "Run on LuaJIT", this, SLOT(onRun()), tr("CTRL+R"), false );
    addDebugMenu(pop);
    addTopCommands(pop);

    new Gui::AutoShortcut( tr("CTRL+O"), this, this, SLOT(onOpenPro()) );
    new Gui::AutoShortcut( tr("CTRL+N"), this, this, SLOT(onNewPro()) );
    new Gui::AutoShortcut( tr("CTRL+SHIFT+N"), this, this, SLOT(onNewModule()) );
    new Gui::AutoShortcut( tr("CTRL+SHIFT+S"), this, this, SLOT(onSavePro()) );
    new Gui::AutoShortcut( tr("CTRL+S"), this, this, SLOT(onSaveFile()) );
    new Gui::AutoShortcut( tr("CTRL+R"), this, this, SLOT(onRun()) );
    new Gui::AutoShortcut( tr("CTRL+SHIFT+R"), this, this, SLOT(onRestartLua()) );
    new Gui::AutoShortcut( tr("CTRL+T"), this, this, SLOT(onCompile()) );
    new Gui::AutoShortcut( tr("CTRL+SHIFT+T"), this, this, SLOT(onGenerate()) );
    new Gui::AutoShortcut( tr(OBN_GOBACK_SC), this, this, SLOT(handleGoBack()) );
    new Gui::AutoShortcut( tr(OBN_GOFWD_SC), this, this, SLOT(handleGoForward()) );
    new Gui::AutoShortcut( tr(OBN_TOGBP_SC), this, this, SLOT(onToggleBreakPt()) );
    new Gui::AutoShortcut( tr(OBN_ENDBG_SC), this, this, SLOT(onEnableDebug()) );
}

void Ide::createMenuBar()
{
    Gui::AutoMenu* pop = new Gui::AutoMenu( tr("File"), this );
    pop->addCommand( "New Project", this, SLOT(onNewPro()), tr("CTRL+N"), false );
    pop->addCommand( "Open Project...", this, SLOT(onOpenPro()), tr("CTRL+O"), false );
    pop->addCommand( "Save Project", this, SLOT(onSavePro()), tr("CTRL+SHIFT+S"), false );
    pop->addCommand( "Save Project as...", this, SLOT(onSaveAs()) );
    pop->addSeparator();
    pop->addCommand( "Save", this, SLOT(onSaveFile()), tr("CTRL+S"), false );
    pop->addCommand( tr("Close file"), d_tab, SLOT(onCloseDoc()), tr("CTRL+W") );
    pop->addCommand( tr("Close all"), d_tab, SLOT(onCloseAll()) );
    pop->addSeparator();
    pop->addCommand( "Export current bytecode...", this, SLOT(onExportBc()) );
    pop->addCommand( "Export all bytecode...", this, SLOT(onExportAllBc()) );
    pop->addSeparator();
    pop->addAutoCommand( "Print...", SLOT(handlePrint()), tr("CTRL+P"), true );
    pop->addAutoCommand( "Export PDF...", SLOT(handleExportPdf()), tr("CTRL+SHIFT+P"), true );
    pop->addSeparator();
    pop->addAction(tr("Quit"),qApp,SLOT(quit()), tr("CTRL+Q") );

    pop = new Gui::AutoMenu( tr("Edit"), this );
    pop->addAutoCommand( "Undo", SLOT(handleEditUndo()), tr("CTRL+Z"), true );
    pop->addAutoCommand( "Redo", SLOT(handleEditRedo()), tr("CTRL+Y"), true );
    pop->addSeparator();
    pop->addAutoCommand( "Cut", SLOT(handleEditCut()), tr("CTRL+X"), true );
    pop->addAutoCommand( "Copy", SLOT(handleEditCopy()), tr("CTRL+C"), true );
    pop->addAutoCommand( "Paste", SLOT(handleEditPaste()), tr("CTRL+V"), true );
    pop->addSeparator();
    pop->addAutoCommand( "Find...", SLOT(handleFind()), tr("CTRL+F"), true );
    pop->addAutoCommand( "Find again", SLOT(handleFindAgain()), tr("F3"), true );
    pop->addAutoCommand( "Replace...", SLOT(handleReplace()) );
    pop->addSeparator();
    pop->addAutoCommand( "&Go to line...", SLOT(handleGoto()), tr("CTRL+G"), true );
    pop->addSeparator();
    pop->addAutoCommand( "Indent", SLOT(handleIndent()) );
    pop->addAutoCommand( "Unindent", SLOT(handleUnindent()) );
    pop->addAutoCommand( "Fix Indents", SLOT(handleFixIndent()) );
    pop->addAutoCommand( "Set Indentation Level...", SLOT(handleSetIndent()) );

    pop = new Gui::AutoMenu( tr("Project"), this );
    pop->addCommand( "New Module...", this, SLOT(onNewModule()), tr("CTRL+SHIFT+N"), false );
    pop->addCommand( "Add existing Modules...", this, SLOT(onAddFiles()) );
    pop->addCommand( "Remove Module...", this, SLOT(onRemoveFile()) );
    pop->addSeparator();
    pop->addCommand( "Built-in Oakwood", this, SLOT(onOakwood()) );
    pop->addCommand( "Set Configuration Variables...", this, SLOT( onSetOptions()) );
    pop->addCommand( "File System Root...", this, SLOT( onWorkingDir() ) );

    pop = new Gui::AutoMenu( tr("Build && Run"), this );
    pop->addCommand( "Compile", this, SLOT(onCompile()), tr("CTRL+T"), false );
    pop->addCommand( "Compile && Generate", this, SLOT(onGenerate()), tr("CTRL+SHIFT+T"), false );
    pop->addCommand( "JIT Enabled", this, SLOT(onJitEnabled()) );
    pop->addCommand( "Restart LuaJIT", this, SLOT(onRestartLua()), tr("CTRL+SHIFT+R"), false );
    pop->addCommand( "Set Command...", this, SLOT(onRunCommand()) );
    pop->addCommand( "Run on LuaJIT", this, SLOT(onRun()), tr("CTRL+R"), false );

    pop = new Gui::AutoMenu( tr("Debug"), this );
    pop->addCommand( "Enable Debugging", this, SLOT(onEnableDebug()),tr(OBN_ENDBG_SC), false );
    pop->addCommand( "Row/Column mode", this, SLOT(onRowColMode()) );
    pop->addCommand( "Bytecode mode", this, SLOT(onBcDebug()) );
    pop->addCommand( "Toggle Breakpoint", this, SLOT(onToggleBreakPt()), tr(OBN_TOGBP_SC), false);
    pop->addAction( d_dbgStepIn );
    pop->addAction( d_dbgStepOver );
    pop->addAction( d_dbgStepOut );
    pop->addAction( d_dbgBreak );
    pop->addAction( d_dbgContinue );
    pop->addAction( d_dbgAbort );


    pop = new Gui::AutoMenu( tr("Window"), this );
    pop->addCommand( tr("Next Tab"), d_tab, SLOT(onDocSelect()), tr(OBN_NEXTDOC_SC) );
    pop->addCommand( tr("Previous Tab"), d_tab, SLOT(onDocSelect()), tr(OBN_PREVDOC_SC) );
    pop->addSeparator();
    pop->addCommand( "Go Back", this, SLOT(handleGoBack()), tr(OBN_GOBACK_SC), false );
    pop->addCommand( "Go Forward", this, SLOT(handleGoForward()), tr(OBN_GOFWD_SC), false );
    pop->addSeparator();
    pop->addAutoCommand( "Set &Font...", SLOT(handleSetFont()) );
    pop->addAutoCommand( "Show &Linenumbers", SLOT(handleShowLinenumbers()) );
    pop->addCommand( "Show Fullscreen", this, SLOT(onFullScreen()) );
    pop->addSeparator();
    QMenu* sub2 = createPopupMenu();
    sub2->setTitle( tr("Show Window") );
    pop->addMenu( sub2 );

    Gui::AutoMenu* help = new Gui::AutoMenu( tr("Help"), this, true );
    help->addCommand( "&About this application...", this, SLOT(onAbout()) );
    help->addCommand( "&About Qt...", this, SLOT(onQt()) );
}

void Ide::onSetOptions()
{
    ENABLED_IF(true);

    QByteArrayList l = d_rt->getPro()->getOptions();
    qSort(l);

    bool ok;
    const QString options = QInputDialog::getMultiLineText(this,tr("Set Configuration Variables"),
                                                           tr("Please enter a unique identifier per variable:"),
                                                           l.join('\n'), &ok );
    if( !ok )
        return;

    Lexer lex;
    QList<Token> toks = lex.tokens(options);
    l.clear();
    QStringList errs;
    foreach( const Token& t, toks )
    {
        if( t.d_type == Tok_ident )
            l << t.d_val;
        else
            errs << QString::fromUtf8(t.d_val);
    }

    if( !errs.isEmpty() )
        QMessageBox::warning(this,tr("Set Configuration Variables"),
                             tr("The following entries are illegal and ignored: \n%1").arg(errs.join('\n')));

    d_rt->getPro()->setOptions(l);
}

void Ide::onCompile()
{
    ENABLED_IF(true);
    compile();
}

void Ide::onRun()
{
    ENABLED_IF( !d_rt->getPro()->getFiles().isEmpty() && !d_rt->getLua()->isExecuting() );

    const bool res = d_rt->run();
    removePosMarkers();
    if( !res )
        onErrors();
}

void Ide::onAbort()
{
    // ENABLED_IF( d_rt->getLua()->isWaiting() );
    d_rt->getLua()->terminate();
}

void Ide::onGenerate()
{
    ENABLED_IF(true);
    compile(true);
}

void Ide::onNewPro()
{
    ENABLED_IF(true);

    if( !checkSaved( tr("New Project")) )
        return;

    // we need a path up front because this path is also the first root path to the source code
    QString fileName = QFileDialog::getSaveFileName(this, tr("New Project"),
                                                          QFileInfo(d_rt->getPro()->getProjectPath()).absolutePath(),
                                                          tr("Luon Project (*.lnpro)") );

    if (fileName.isEmpty())
        return;

    QDir::setCurrent(QFileInfo(fileName).absolutePath());

    if( !fileName.endsWith(".lnpro",Qt::CaseInsensitive ) )
        fileName += ".lnpro";

    d_rt->getPro()->createNew();
    d_tab->onCloseAll();
    compile();


    d_rt->getPro()->saveTo(fileName);

}

void Ide::onOpenPro()
{
    ENABLED_IF( true );

    if( !checkSaved( tr("New Project")) )
        return;

    const QString fileName = QFileDialog::getOpenFileName(this, tr("Open Project"),QString(),
                                                          tr("Luon Project (*.lnpro)") );
    if (fileName.isEmpty())
        return;

    QDir::setCurrent(QFileInfo(fileName).absolutePath());

    d_tab->onCloseAll();
    clear();
    d_rt->getPro()->loadFrom(fileName);

    compile();
}

void Ide::onSavePro()
{
    ENABLED_IF( d_rt->getPro()->isDirty() );

    if( !d_rt->getPro()->getProjectPath().isEmpty() )
        d_rt->getPro()->save();
    else
        onSaveAs();
}

void Ide::onSaveFile()
{
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    ENABLED_IF( edit && edit->isModified() );

    edit->saveToFile( edit->getPath() );
    Project::File* f = d_rt->getPro()->findFile( edit->getPath() );
    if( f )
        f->d_cache.clear();
}

void Ide::onSaveAs()
{
    ENABLED_IF(true);

    QString fileName = QFileDialog::getSaveFileName(this, tr("Save Project"),
                                                          QFileInfo(d_rt->getPro()->getProjectPath()).absolutePath(),
                                                          tr("Luon Project (*.lnpro)") );

    if (fileName.isEmpty())
        return;

    QDir::setCurrent(QFileInfo(fileName).absolutePath());

    if( !fileName.endsWith(".lnpro",Qt::CaseInsensitive ) )
        fileName += ".lnpro";

    d_rt->getPro()->saveTo(fileName);
    onCaption();
}

void Ide::onCaption()
{
    const QString star = d_rt->getPro()->isDirty() || d_filesDirty ? "*" : "";
    if( d_rt->getPro()->getProjectPath().isEmpty() )
    {
        setWindowTitle(tr("<unnamed>%2 - %1").arg(qApp->applicationName()).arg(star));
    }else
    {
        QFileInfo info(d_rt->getPro()->getProjectPath());
        setWindowTitle(tr("%1%2 - %3").arg(info.fileName()).arg(star).arg(qApp->applicationName()) );
    }
}

void Ide::onGotoLnr(quint32 lnr)
{
    if( d_lock )
        return;
    d_lock = true;
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    if( edit )
    {
        const int row = RowCol::unpackRow(lnr)-1;
        const int col = RowCol::unpackCol(lnr)-1;
        edit->setCursorPosition(row,col);
        edit->setFocus();
    }
    d_lock = false;
}

void Ide::onFullScreen()
{
    CHECKED_IF(true,isFullScreen());
    QSettings s;
    if( isFullScreen() )
    {
        showMaximized();
        s.setValue("Fullscreen", false );
    }else
    {
        showFullScreen();
        s.setValue("Fullscreen", true );
    }
}

void Ide::onCursor()
{
    fillXref();
    if( d_lock )
        return;
    d_lock = true;
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    if( edit )
    {
        int row, col;
        edit->getCursorPosition(&row,&col);
        d_bcv->gotoLine(RowCol(row+1,col+1).packed());
    }
    d_lock = false;
}

void Ide::onExportBc()
{
    ENABLED_IF(d_tab->getCurrentTab() != 0 );

    const QString dirPath = QFileDialog::getExistingDirectory(this, tr("Save Binary") );

    if (dirPath.isEmpty())
        return;

    QDir dir(dirPath);

    const QString curPath = d_tab->getCurrentDoc().toString();

    LjRuntime::BytecodeList l = d_rt->findAllByteCodesOfPath(curPath);
    for( int i = 0; i < l.size(); i++ )
    {
        QString path = dir.absoluteFilePath(l[i].first->getModuleFullName(true) + ".lua");
        QFile out(path);
        out.open(QIODevice::WriteOnly);
        out.write(l[i].second);
    }
    if( l.isEmpty() )
        QMessageBox::warning(this,tr("Export Bytecode"), tr("No bytecode was found for given module") );
}

void Ide::onExportAllBc()
{
    ENABLED_IF( d_rt->hasBytecode() && !d_rt->hasBuildErrors() );

    const QString dirPath = QFileDialog::getExistingDirectory(this, tr("Save Bytecode") );

    if (dirPath.isEmpty())
        return;

    d_rt->saveBytecode(dirPath,".lua");
}

void Ide::onModsDblClicked(QTreeWidgetItem* item, int)
{
    Declaration* s = item->data(0,Qt::UserRole).value<Declaration*>();
    if( s )
        showEditor( s );
    else
        showEditor(item->toolTip(0));
}

void Ide::onModDblClicked(QTreeWidgetItem* item, int)
{
    Declaration* s = item->data(0,Qt::UserRole).value<Declaration*>();
    if( s == 0 )
        return;

    d_curModule = s->getModule();
    d_lock2 = true;
    const QString path = d_tab->getCurrentDoc().toString();
    item->setExpanded(true);
    showEditor( path, s->pos.d_row, s->pos.d_col, false, true );
    d_lock2 = false;
}

void Ide::onHierDblClicked(QTreeWidgetItem* item, int)
{
    Declaration* s = item->data(0,Qt::UserRole).value<Declaration*>();
    if( s == 0 )
        return;

    d_lock4 = true;
    Declaration* mod = s->getModule();
    if( mod )
    {
        ModuleData md = mod->data.value<ModuleData>();
        showEditor( md.source, s->pos.d_row, s->pos.d_col, false, true );
        item->setExpanded(true);
    }
    d_lock4 = false;
}

void Ide::onStackDblClicked(QTreeWidgetItem* item, int)
{
    if( item )
    {
        const QString source = item->data(3,Qt::UserRole).toString();
        if( d_rt->getLua()->getMode() == Lua::Engine2::PcMode )
        {
            if( !source.isEmpty() )
            {
                const quint32 line = item->data(2,Qt::UserRole).toUInt();
                const quint32 func = item->data(1,Qt::UserRole).toUInt();
                showEditor(source, RowCol::unpackRow(func), RowCol::unpackCol(func), false );
                d_bcv->parentWidget()->show();
                d_bcv->gotoFuncPc(func,line, false, false);
            }
        }else
        {
            if( !source.isEmpty() )
            {
                const quint32 line = item->data(2,Qt::UserRole).toUInt();
                showEditor( source, RowCol::unpackRow(line), RowCol::unpackCol(line) );
            }
        }
        const int level = item->data(0,Qt::UserRole).toInt();
        d_rt->getLua()->setActiveLevel(level);
        fillLocals();
   }
}

void Ide::onTabChanged()
{
    const QString path = d_tab->getCurrentDoc().toString();

    onEditorChanged();

    if( !path.isEmpty() )
    {
        Project::File* f = d_rt->getPro()->findFile(path);
        if( f )
        {
            fillModule(f->d_mod);
            showBc(d_rt->findByteCode(f->d_mod)); // TODO: what about instances
            onCursor();
            return;
        }
    }
    // else
    d_bcv->clear();
    fillModule(0);
}

void Ide::onTabClosing(int i)
{
    d_rt->getPro()->findFile(d_tab->getDoc(i).toString())->d_cache.clear();
}

void Ide::onEditorChanged()
{
    // only fired once when editor switches from unmodified to modified and back
    // not fired for every key press
    d_filesDirty = false;
    for( int i = 0; i < d_tab->count(); i++ )
    {
        Editor* e = static_cast<Editor*>( d_tab->widget(i) );
        if( e->isModified() )
            d_filesDirty = true;
        const QString path = d_tab->getDoc(i).toString();
        Project::File* f = d_rt->getPro()->findFile(path);
        QString name;
        if( f && f->d_mod )
            name = f->d_mod->name;
        else
            name = QFileInfo( path ).fileName();
        d_tab->setTabText(i, name + ( e->isModified() ? "*" : "" ) );
        d_tab->setTabToolTip( i, path );
    }
    onCaption();
}

void Ide::onErrorsDblClicked()
{
    QTreeWidgetItem* item = d_errs->currentItem();
    showEditor( item->data(0, Qt::UserRole ).toString(),
                item->data(1, Qt::UserRole ).toInt(), item->data(2, Qt::UserRole ).toInt() );
}

void Ide::onErrors()
{
    d_errs->clear();
    QList<Project::Error> errs = d_rt->getPro()->getErrors();

    for( int i = 0; i < errs.size(); i++ )
    {
        QTreeWidgetItem* item = new QTreeWidgetItem(d_errs);
        item->setText(2, errs[i].msg );
        item->setToolTip(2, item->text(2) );
        //if( errs[i].d_isErr )
            item->setIcon(0, QPixmap(":/images/exclamation-red.png") );
#if 0
        else
            item->setIcon(0, QPixmap(":/images/exclamation-circle.png") );
#endif
        Project::File* f = d_rt->getPro()->findFile(errs[i].path);
        if( f && f->d_mod )
            item->setText(0, f->d_mod->name );
        else
            item->setText(0, QFileInfo(errs[i].path).completeBaseName() );
        item->setToolTip(0, errs[i].path );
        item->setText(1, QString("%1:%2").arg(errs[i].pos.d_row).arg(errs[i].pos.d_col));
        item->setData(0, Qt::UserRole, errs[i].path );
        item->setData(1, Qt::UserRole, errs[i].pos.d_row );
        item->setData(2, Qt::UserRole, errs[i].pos.d_col );
    }
    if( errs.size() )
        d_errs->parentWidget()->show();

    for( int i = 0; i < d_tab->count(); i++ )
    {
        Editor* e = static_cast<Editor*>( d_tab->widget(i) );
        Q_ASSERT( e );
        e->updateExtraSelections();
    }
}

void Ide::onOpenFile()
{
    ENABLED_IF( d_mods->currentItem() );

    onModsDblClicked( d_mods->currentItem(), 0 );
}

void Ide::onOakwood()
{
    CHECKED_IF( true, d_rt->getPro()->useBuiltInOakwood() );

    d_rt->getPro()->setUseBuiltInOakwood( !d_rt->getPro()->useBuiltInOakwood() );
}

void Ide::onAddFiles()
{
    ENABLED_IF(true);

    QByteArrayList path;

    QList<QTreeWidgetItem*> sel = d_mods->selectedItems();
    if( sel.size() == 1 && sel.first()->type() == 1 )
        path = sel.first()->text(0).toLatin1().split('.');

    QString filter;
    foreach( const QString& suf, d_rt->getPro()->getSuffixes() )
        filter += " *" + suf;
    const QStringList files = QFileDialog::getOpenFileNames(this,tr("Add Modules"),QString(),filter );
    foreach( const QString& f, files )
    {
        if( !d_rt->getPro()->addFile(f,path) )
            qWarning() << "cannot add module" << f;
    }
    compile();
}

void Ide::onNewModule()
{
    ENABLED_IF(true);

    QByteArrayList path;

    QList<QTreeWidgetItem*> sel = d_mods->selectedItems();
    if( sel.size() == 1 && sel.first()->type() == 1 )
        path = sel.first()->text(0).toLatin1().split('.');

    const QByteArray name = QInputDialog::getText(this,tr("New Module"), tr("Enter a unique module name:") ).toLatin1();
    if( name.isEmpty() )
        return;

    if( !Lexer::isValidIdent(name) )
    {
        QMessageBox::critical(this,tr("New Module"), tr("'%1' is not a valid module name").arg(name.constData()) );
        return;
    }
    QDir dir;
    const Project::FileGroup* fg = d_rt->getPro()->findFileGroup(path);
    for( int i = 0; i < fg->d_files.size(); i++ )
    {
        if( i == 0 )
            dir = fg->d_files[i]->d_filePath;
        if( fg->d_files[i]->d_mod != 0 && fg->d_files[i]->d_mod->name == name )
        {
            QString where;
            if( !path.isEmpty() )
                where = " in " + path.join('.');
            QMessageBox::critical(this,tr("New Module"), tr("'%1' is not unique%2")
                                  .arg(name.constData()).arg(where) );
            return;
        }
    }

    QString filePath = QFileDialog::getSaveFileName(this,tr("New Module"), dir.absoluteFilePath(name + ".luon"),"*.luon");
    if( filePath.isEmpty() )
        return;

    if( !filePath.toLower().endsWith(".luon") )
        filePath += ".luon";

    QFile f(filePath);
    if( !f.open(QIODevice::WriteOnly) )
    {
        QMessageBox::critical(this,tr("New Module"), tr("Cannot open file for writing: '%1'").arg(filePath) );
        return;
    }
    f.write("module ");
    f.write(name);
    f.write("\n\n\n");
    f.write("end ");
    f.write(name);
    f.write("\n");
    f.close();

    if( !d_rt->getPro()->addFile(filePath,path) )
        qWarning() << "cannot add module" << filePath;
    compile();
}

void Ide::onAddDir()
{
    ENABLED_IF(true);

    bool ok;
    const QByteArray path = QInputDialog::getText(this,tr("Add Inport Path"), tr("Idents separated by '.':"),
                                                  QLineEdit::Normal, QString(), &ok ).toLatin1();
    if( !ok )
        return;
    if( !path.isEmpty() && !::isalpha( path[0] ) )
    {
        QMessageBox::critical(this,tr("Add Inport Path"),tr("import path starts with invalid character: %1").arg(path[0]));
        return;
    }
    for( int i = 1; i < path.size(); i++ )
    {
        const char ch = path[i];
        if( !::isalnum(ch) && ch != '.' )
        {
            QMessageBox::critical(this,tr("Add Inport Path"),tr("invalid character in import path: %1").arg(ch));
            return;
        }
    }
    QByteArrayList segments = path.split('.');
    for( int i = 0; i < segments.size(); i++ )
    {
        if( segments[i].isEmpty() )
        {
            QMessageBox::critical(this,tr("Add Inport Path"),tr("identifier cannot be empty"));
            return;
        }
    }
    d_rt->getPro()->addPackagePath(segments);
    fillMods();
}

void Ide::onRemoveFile()
{
    ENABLED_IF( d_mods->currentItem() && d_mods->currentItem()->type() == 0 );

    Project::File* f = d_mods->currentItem()->data(0,Qt::UserRole).value<Project::File*>();
    if( f == 0 )
        return;

    if( QMessageBox::warning( this, tr("Remove Module"),
                              tr("Do you really want to remove module '%1' from project?").arg(f->d_name.constData()),
                           QMessageBox::Yes | QMessageBox::Cancel, QMessageBox::Yes ) != QMessageBox::Yes )
        return;

    for( int i = 0; i < d_tab->count(); i++ )
    {
        Editor* e = static_cast<Editor*>( d_tab->widget(i) );
        if( e->getPath() == f->d_filePath )
        {
            d_tab->closeTab(i);
            break;
        }
    }

    if( !d_rt->getPro()->removeFile( f->d_filePath ) )
        qWarning() << "cannot remove module" << f->d_name;
    else
        compile();
}

void Ide::onRemoveDir()
{
    ENABLED_IF( d_mods->currentItem() && d_mods->currentItem()->type() == 1
                && d_mods->currentItem()->childCount() == 0 );

    if( QMessageBox::warning( this, tr("Remove Import Path"),
                              tr("Do you really want to remove '%1' from project?").arg(d_mods->currentItem()->text(0)),
                           QMessageBox::Yes | QMessageBox::Cancel, QMessageBox::Yes ) != QMessageBox::Yes )
        return;
    QByteArrayList path = d_mods->currentItem()->text(0).toLatin1().split('.');
    if( !d_rt->getPro()->removePackagePath( path ) )
        qWarning() << "cannot remove import path" << d_mods->currentItem()->text(0);
    fillMods();
}

void Ide::onEnableDebug()
{
    CHECKED_IF( true, d_rt->getLua()->isDebug() );

    d_rt->getLua()->setDebug( !d_rt->getLua()->isDebug() );
    enableDbgMenu();
}

void Ide::onBreak()
{
    // normal call because called during processEvent which doesn't seem to enable
    // the functions: ENABLED_IF( d_rt->getLua()->isExecuting() );
    d_rt->getLua()->runToNextLine();
}

bool Ide::checkSaved(const QString& title)
{
    if( d_filesDirty )
    {
        switch( QMessageBox::critical( this, title, tr("There are modified files; do you want to save them?"),
                               QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel, QMessageBox::Yes ) )
        {
        case QMessageBox::Yes:
            // TODO
            break;
        case QMessageBox::No:
            break;
        default:
            return false;
        }
    }
    if( d_rt->getPro()->isDirty() )
    {
        switch( QMessageBox::critical( this, title, tr("The the project has not been saved; do you want to save it?"),
                               QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel, QMessageBox::Yes ) )
        {
        case QMessageBox::Yes:
            if( !d_rt->getPro()->getProjectPath().isEmpty() )
                return d_rt->getPro()->save();
            else
            {
                const QString path = QFileDialog::getSaveFileName( this, title, QString(), "Luon Project (*.lnpro)" );
                if( path.isEmpty() )
                    return false;
                QDir::setCurrent(QFileInfo(path).absolutePath());
                return d_rt->getPro()->saveTo(path);
            }
            break;
        case QMessageBox::No:
            return true;
        default:
            return false;
        }
    }
    return true;
}

bool Ide::compile(bool doGenerate )
{
    for( int i = 0; i < d_tab->count(); i++ )
    {
        Editor* e = static_cast<Editor*>( d_tab->widget(i) );
        Project::File* f = d_rt->getPro()->findFile( e->getPath() );
        if( f == 0 )
            continue;
        if( e->isModified() )
            f->d_cache = e->toPlainText().toUtf8();
        else
            f->d_cache.clear();
    }
    const bool res = d_rt->compile(doGenerate);
    onErrors();
    fillMods();
    fillModule(0);
    fillHier(0);
    fillXref();
    onTabChanged();
    return true;
}

static bool sortNamed( const Project::File* lhs, const Project::File* rhs )
{
    QByteArray l;
    l = lhs->d_name;
    QByteArray r;
    r = rhs->d_name;
    return l.toLower() < r.toLower();
}

typedef QPair<QByteArray,QList<Project::File*> > Group;

static bool sortNamed1( const Group& lhs, const Group& rhs )
{
    return lhs.first.toLower() < rhs.first.toLower();
}

typedef QList<Project::File*> ModuleSort;

template<class T>
static void fillModTree( T* parent, const ModuleSort& mods )
{
    foreach( Project::File* m, mods )
    {
        QTreeWidgetItem* item = new QTreeWidgetItem(parent);
        if( m->d_mod )
            item->setText(0,m->d_mod->name);
        else
            item->setText(0, m->d_name.constData() );
        item->setToolTip(0,m->d_filePath);
#if 0
        if( m->d_isDef )
            item->setIcon(0, QPixmap(":/images/definition.png") );
        else
#endif
            item->setIcon(0, QPixmap(":/images/module.png") );
        item->setData(0,Qt::UserRole,QVariant::fromValue(m) );
    }
}

void Ide::fillMods()
{
    d_mods->clear();

    const Project::FileGroups& paths = d_rt->getPro()->getFileGroups();
    typedef QList<Group> Sort1;
    Sort1 sort1;
    foreach( const Project::FileGroup& fg, paths )
        sort1.append( qMakePair( fg.d_package.join('.'), fg.d_files ) );
    std::sort( sort1.begin(), sort1.end(), sortNamed1 );

    for( int j = 0; j < sort1.size(); j++ )
    {
        QTreeWidgetItem* item = 0;
        if( !sort1[j].first.isEmpty() )
        {
            item = new QTreeWidgetItem(d_mods,1);
            item->setText(0, sort1[j].first);
            item->setToolTip( 0, item->text(0) );
            item->setIcon(0, QPixmap(":/images/folder.png") );
        }

        const QList<Project::File*>& files = sort1[j].second;
        ModuleSort sort;
        for( int i = 0; i < files.size(); i++ )
            sort << files[i];
        std::sort( sort.begin(), sort.end(), sortNamed );
        if( item )
            fillModTree(item,sort);
        else
            fillModTree( d_mods, sort );
    }
    d_mods->expandAll();
}

void Ide::addTopCommands(Gui::AutoMenu* pop)
{
    Q_ASSERT( pop != 0 );
    pop->addSeparator();
    pop->addCommand( "Go Back", this, SLOT(handleGoBack()), tr(OBN_GOBACK_SC), false );
    pop->addCommand( "Go Forward", this, SLOT(handleGoForward()), tr(OBN_GOFWD_SC), false );
    pop->addSeparator();
    pop->addAutoCommand( "Set &Font...", SLOT(handleSetFont()) );
    pop->addAutoCommand( "Show &Linenumbers", SLOT(handleShowLinenumbers()) );
    pop->addCommand( "Show Fullscreen", this, SLOT(onFullScreen()) );
    pop->addSeparator();
    pop->addAction(tr("Quit"),qApp,SLOT(quit()) );
}

Ide::Editor* Ide::showEditor(const QString& path, int row, int col, bool setMarker, bool center )
{
    QString filePath = path;
    Project::File* f = d_rt->getPro()->findFile(path);
    if( f == 0 )
        return 0;
    filePath = f->d_filePath;

    const int i = d_tab->findDoc(filePath);
    Editor* edit = 0;
    if( i != -1 )
    {
        d_tab->setCurrentIndex(i);
        edit = static_cast<Editor*>( d_tab->widget(i) );
    }else
    {
        edit = new Editor(this,d_rt->getPro());
        createMenu(edit);

        connect(edit, SIGNAL(modificationChanged(bool)), this, SLOT(onEditorChanged()) );
        connect(edit,SIGNAL(cursorPositionChanged()),this,SLOT(onCursor()));
        connect(edit,SIGNAL(sigUpdateLocation(int,int)),this,SLOT(onUpdateLocation(int,int)));

        edit->setExt(true);
        edit->loadFromFile(filePath);

        if( f && f->d_mod )
        {
            const Lua::Engine2::Breaks& br = d_rt->getLua()->getBreaks( f->d_mod->name );
            Lua::Engine2::Breaks::const_iterator j;
            for( j = br.begin(); j != br.end(); ++j )
                edit->addBreakPoint((*j) - 1);
        }

        d_tab->addDoc(edit,filePath);
        onEditorChanged();
    }
    QByteArray bc = d_rt->findByteCode(path.toUtf8());
    if( bc.isEmpty() && f && f->d_mod )
        bc = d_rt->findByteCode(f->d_mod);
    showBc( bc );
    if( row > 0 && col > 0 )
    {
        edit->setCursorPosition( row-1, col-1, center );
        if( setMarker )
            edit->setPositionMarker(row-1);
    }
    edit->setFocus();
    return edit;
}

void Ide::showEditor(Declaration* n, bool setMarker, bool center)
{
    if( n == 0 )
        return;
    Declaration* mod = n->getModule();
    if( mod )
    {
        d_curModule = mod;
        ModuleData md = mod->data.value<ModuleData>();
        showEditor( md.source, n->pos.d_row, n->pos.d_col, setMarker, center );
    }
}

void Ide::showEditor(const Ide::Location& loc)
{
    Editor* e = showEditor( loc.d_file, loc.d_line+1, loc.d_col+1 );
    if( e )
        e->verticalScrollBar()->setValue(loc.d_yoff);
}

void Ide::createMenu(Ide::Editor* edit)
{
    Gui::AutoMenu* pop = new Gui::AutoMenu( edit, true );
    pop->addCommand( "Save", this, SLOT(onSaveFile()), tr("CTRL+S"), false );
    pop->addSeparator();
    pop->addCommand( "Compile", this, SLOT(onCompile()), tr("CTRL+T"), false );
    pop->addCommand( "Run on LuaJIT", this, SLOT(onRun()), tr("CTRL+R"), false );
    addDebugMenu(pop);
    pop->addSeparator();
    pop->addCommand( "Export current bytecode...", this, SLOT(onExportBc()) );
    pop->addSeparator();
    pop->addCommand( "Undo", edit, SLOT(handleEditUndo()), tr("CTRL+Z"), true );
    pop->addCommand( "Redo", edit, SLOT(handleEditRedo()), tr("CTRL+Y"), true );
    pop->addSeparator();
    pop->addCommand( "Cut", edit, SLOT(handleEditCut()), tr("CTRL+X"), true );
    pop->addCommand( "Copy", edit, SLOT(handleEditCopy()), tr("CTRL+C"), true );
    pop->addCommand( "Paste", edit, SLOT(handleEditPaste()), tr("CTRL+V"), true );
    pop->addSeparator();
    pop->addCommand( "Find...", edit, SLOT(handleFind()), tr("CTRL+F"), true );
    pop->addCommand( "Find again", edit, SLOT(handleFindAgain()), tr("F3"), true );
    pop->addCommand( "Replace...", edit, SLOT(handleReplace()) );
    pop->addSeparator();
    pop->addCommand( "&Goto...", edit, SLOT(handleGoto()), tr("CTRL+G"), true );
    pop->addSeparator();
    pop->addCommand( "Indent", edit, SLOT(handleIndent()) );
    pop->addCommand( "Unindent", edit, SLOT(handleUnindent()) );
    pop->addCommand( "Fix Indents", edit, SLOT(handleFixIndent()) );
    pop->addCommand( "Set Indentation Level...", edit, SLOT(handleSetIndent()) );
    pop->addSeparator();
    pop->addCommand( "Print...", edit, SLOT(handlePrint()), tr("CTRL+P"), true );
    pop->addCommand( "Export PDF...", edit, SLOT(handleExportPdf()), tr("CTRL+SHIFT+P"), true );
    addTopCommands(pop);
}

void Ide::addDebugMenu(Gui::AutoMenu* pop)
{
    Gui::AutoMenu* sub = new Gui::AutoMenu(tr("Debugger"), this, false );
    pop->addMenu(sub);
    sub->addCommand( "Enable Debugging", this, SLOT(onEnableDebug()),tr(OBN_ENDBG_SC), false );
    sub->addCommand( "Toggle Breakpoint", this, SLOT(onToggleBreakPt()), tr(OBN_TOGBP_SC), false);
    sub->addAction( d_dbgStepIn );
    sub->addAction( d_dbgStepOver );
    sub->addAction( d_dbgStepOut );
    sub->addAction( d_dbgBreak );
    sub->addAction( d_dbgContinue );
    sub->addAction( d_dbgAbort );

}

bool Ide::luaRuntimeMessage(const QByteArray& msg, const QString& file )
{
    // TODO use Engine2::decodeRuntimeMessage
    const int rbrack = msg.indexOf(']'); // cannot directly search for ':' because Windows "C:/"
    if( rbrack == -1 )
        return false;
    const int firstColon = msg.indexOf(':', rbrack);
    if( firstColon != -1 )
    {
        const int secondColon = msg.indexOf(':',firstColon + 1);
        if( secondColon != -1 )
        {
            QString path = msg.left(firstColon);
            const int firstTick = path.indexOf('"');
            if( firstTick != -1 )
            {
                const int secondTick = path.indexOf('"',firstTick+1);
                path = path.mid(firstTick+1,secondTick-firstTick-1);
            }else
                path.clear();
            const quint32 id = msg.mid(firstColon+1, secondColon - firstColon - 1 ).toInt(); // lua deliveres negative numbers
            const bool packed = RowCol::isPacked(id);
            const quint32 row = packed ? RowCol::unpackRow(id) : id;
            const quint32 col = packed ? RowCol::unpackCol(id) : 1;

            d_rt->getPro()->addError(path.isEmpty() ? // Errors::Runtime,
                                                 file : path, RowCol(row, col), msg.mid(secondColon+1) );
            return true;
        }
    }
    return false;
    // qWarning() << "Unknown Lua error message format:" << msg;
}

static bool sortExList( const Symbol* lhs, Symbol* rhs )
{
    Declaration* lm = lhs->decl;
    Declaration* rm = rhs->decl;
    const QByteArray ln = lm ? lm->name : QByteArray();
    const QByteArray rn = rm ? rm->name : QByteArray();
    const quint32 ll = lhs->pos.packed();
    const quint32 rl = rhs->pos.packed();

    return ln < rn || (!(rn < ln) && ll < rl);
}

static const char* roleName( Symbol* e )
{
    switch( e->kind )
    {
    case Symbol::Decl:
    case Symbol::Module:
        return "decl";
    case Symbol::Lval:
        return "lhs";
    default:
        break;
    }
    return "";
}

static Declaration* adjustForModIdx( Declaration* decl )
{
    while( decl )
    {
        switch( decl->kind )
        {
        case Declaration::Procedure:
        case Declaration::Module:
            return decl;
        }
        decl = decl->outer;
    }
    return 0;
}

void Ide::fillXref()
{
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    if( edit == 0 )
    {
        d_xref->clear();
        d_xrefTitle->clear();
        return;
    }
    int line, col;
    edit->getCursorPosition( &line, &col );
    line += 1;
    col += 1;
    Declaration* scope = 0;
    Symbol* hit = d_rt->getPro()->findSymbolBySourcePos(edit->getPath(), line, col, &scope);
    if( hit && hit->decl )
    {
        Declaration* module = moduleOfCurrentEditor();
        fillXrefForSym(hit, module);
        syncModView(hit->decl);
        syncEditorMarks(hit->decl, module);
        fillHier(hit->decl);
    }else
        edit->markNonTerms(SymList());
}

static inline QString declKindName(Declaration* decl)
{
    switch( decl->kind )
    {
    case Declaration::Field:
        return "Field";
    case Declaration::VarDecl:
    case Declaration::LocalDecl:
        return "Variable";
    case Declaration::ParamDecl:
        return "Parameter";
    case Declaration::TypeDecl:
        return "Type";
    case Declaration::ConstDecl:
        if( decl->mode == Declaration::Enum )
            return "Enumeration";
        else
            return "Const";
    case Declaration::Import:
        return "Import";
    case Declaration::Builtin:
        return "BuiltIn";
    case Declaration::Procedure:
        return "Procedure";
    case Declaration::Module:
        return "Module";
    }
    return "???";
}

void Ide::fillXrefForSym(Symbol* sym, Declaration* module)
{
    d_xref->clear();

    Declaration* decl = sym->decl;
    Q_ASSERT( decl != 0 );

    Project::UsageByMod usage = d_rt->getPro()->getUsage(decl);

    QFont f = d_xref->font();
    f.setBold(true);

    const QString type = declKindName(decl);

    d_xrefTitle->setText(QString("%1 '%2'").arg(type).arg(decl->name.constData()));

    QTreeWidgetItem* black = 0;
    for( int i = 0; i < usage.size(); i++ )
    {
        SymList syms = usage[i].second;
        std::sort( syms.begin(), syms.end(), sortExList );
        QString modName;
        if( usage[i].first )
            modName = usage[i].first->name;
        else
            modName = "<global>";
        foreach( Symbol* s, syms )
        {
            QTreeWidgetItem* item = new QTreeWidgetItem(d_xref);
            item->setText( 0, QString("%1 (%2:%3 %4)")
                        .arg(modName)
                        .arg(s->pos.d_row).arg(s->pos.d_col)
                        .arg( roleName(s) ));
            if( s == sym )
            {
                item->setFont(0,f);
                black = item;
            }
            item->setToolTip( 0, item->text(0) );
            item->setData( 0, Qt::UserRole, QVariant::fromValue( s ) ); // symbol in module
            item->setData( 1, Qt::UserRole, QVariant::fromValue( usage[i].first ) ); // module where the sym was found
            if( usage[i].first != module )
                item->setForeground( 0, Qt::gray );
        }
    }
    if( black && !d_lock3 )
    {
        d_xref->scrollToItem(black, QAbstractItemView::PositionAtCenter);
        d_xref->setCurrentItem(black);
    }
}

void Ide::syncModView(Declaration* decl)
{
    QTreeWidgetItem* mi = d_modIdx.value(decl);
    if( mi == 0 )
        mi = d_modIdx.value(adjustForModIdx(decl));
    if( mi && !d_lock2 )
    {
        d_mod->scrollToItem(mi,QAbstractItemView::PositionAtCenter);
        mi->setExpanded(true);
        d_mod->setCurrentItem(mi);
    }
}

void Ide::syncEditorMarks(Declaration* selected, Declaration* module)
{
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    if( edit == 0 )
        return;
    Symbol* syms = d_rt->getPro()->getSymbolsOfModule(module);
    if( syms == 0 )
    {
        edit->markNonTerms(SymList());
        return;
    }

    Symbol* s = syms;
    SymList marks;
    while( s )
    {
        if( s->decl == selected )
            marks << s;
        s = s->next;
        if( s == syms )
            break;
    }

    edit->markNonTerms(marks);
}

Declaration*Ide::moduleOfCurrentEditor()
{
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    if( edit == 0 )
        return 0;
    Project::File* f = d_rt->getPro()->findFile(edit->getPath());
    if( f == 0 )
        return 0;
    return f->d_mod;
}

void Ide::fillStack()
{
    d_stack->clear();

    Lua::Engine2::StackLevels ls = d_rt->getLua()->getStackTrace();
    d_scopes = QVector<Declaration*>(ls.size());

    bool opened = false;
    for( int level = 0; level < ls.size(); level++ )
    {
        const Lua::Engine2::StackLevel& l = ls[level];
        // Level, Name, Pos, Mod
        QTreeWidgetItem* item = new QTreeWidgetItem(d_stack);
        item->setText(0,QString::number(l.d_level));
        item->setData(0,Qt::UserRole,l.d_level);
        item->setText(1,l.d_name);
        if( l.d_inC )
        {
            item->setText(3,"(native)");
        }else if( d_rt->getLua()->getMode() != Lua::Engine2::PcMode )
        {
            const int row = RowCol::unpackRow(l.d_line);
            const int col = RowCol::unpackCol(l.d_line);
            const int row2 = RowCol::unpackRow(l.d_lineDefined);
            const int col2 = RowCol::unpackCol(l.d_lineDefined);
            Symbol* e = d_rt->getPro()->findSymbolByModuleName(l.d_source,row2,col2);
            Declaration* ident = e ? e->decl : 0;
            if( ident )
            {
                if( ident->kind == Declaration::Procedure || ident->kind == Declaration::Module )
                {
                    item->setText(1,ident->name );
                    d_scopes[level] = ident;
                }
            }else
                item->setText(1,"<unknown>" );
            //qDebug() << "level" << level << ( d_scopes[level] ? d_scopes[level]->getModule()->name : QByteArray("???") );
            item->setText(2,QString("%1:%2").arg(row).arg(col));
            item->setData(2, Qt::UserRole, l.d_line );
            if( ident )
                item->setText(3, ident->getModule()->getModuleFullName(true) );
            else
                item->setText(3, l.d_source );
            item->setData(3, Qt::UserRole, l.d_source );
            item->setToolTip(3, l.d_source );
            if( !opened )
            {
                showEditor(l.d_source, row, col, true );
                d_rt->getLua()->setActiveLevel(level);
                opened = true;
            }
        }else
        {
            item->setData(1,Qt::UserRole,l.d_lineDefined );
            item->setText(2,QString("%1").arg( l.d_line - 1 ));
            item->setData(2, Qt::UserRole, l.d_line );
            item->setText(3, l.d_source );
            item->setData(3, Qt::UserRole, l.d_source );
            item->setToolTip(3, l.d_source );
            if( !opened )
            {
                Editor* edit = showEditor(l.d_source, RowCol::unpackRow(l.d_lineDefined),
                                          RowCol::unpackCol(l.d_lineDefined), false );
                d_rt->getLua()->setActiveLevel(level);
                d_bcv->parentWidget()->show();
                const quint32 rowCol = d_bcv->gotoFuncPc(l.d_lineDefined,l.d_line, center, true);
                if( rowCol && edit )
                {
                    d_lock = true;
                    edit->dbgRow = RowCol::unpackRow(rowCol) - 1;
                    edit->dbgCol = RowCol::unpackCol(rowCol) - 1;
                    edit->setCursorPosition( edit->dbgRow, edit->dbgCol, center );
                    edit->setPositionMarker(edit->dbgRow);
                    d_lock = false;
                }
                opened = true;
            }
        }
    }

    d_stack->parentWidget()->show();
}

static void typeAddr( QTreeWidgetItem* item, const QVariant& val )
{
    if( val.canConvert<Lua::Engine2::VarAddress>() )
    {
        Lua::Engine2::VarAddress addr = val.value<Lua::Engine2::VarAddress>();
        if( addr.d_addr )
            item->setToolTip(1, QString("address 0x%1").arg(ptrdiff_t(addr.d_addr),8,16,QChar('0')));
        switch( addr.d_type )
        {
        case Lua::Engine2::LocalVar::NIL:
            item->setText(1, "nil");
            break;
        case Lua::Engine2::LocalVar::FUNC:
            item->setText(1, "func");
            break;
        case Lua::Engine2::LocalVar::TABLE:
            item->setText(1, "table");
            break;
        case Lua::Engine2::LocalVar::STRUCT:
            item->setText(1, "struct");
            break;
        }
    }else if( val.type() == QMetaType::QVariantMap)
    {
        QVariantMap map = val.toMap();
        typeAddr( item, map.value(QString()) );
    }
}

static void fillLocalSubs( QTreeWidgetItem* super, const QVariantMap& vals )
{
    QVariantMap::const_iterator i;
    for( i = vals.begin(); i != vals.end(); i++ )
    {
        if( i.key().isEmpty() )
            continue;
        QTreeWidgetItem* item = new QTreeWidgetItem(super);
        item->setText(0, i.key() );

        if( i.value().canConvert<Lua::Engine2::VarAddress>() )
        {
            typeAddr(item,i.value());
        }else if( i.value().type() == QMetaType::QVariantMap)
        {
            typeAddr(item,i.value());
            fillLocalSubs(item,i.value().toMap() );
        }else if( i.value().type() == QMetaType::QByteArray )
        {
            item->setText(1, "\"" + i.value().toString().simplified() + "\"");
            item->setToolTip(1, i.value().toString());
        }else
            item->setText(1,i.value().toString());
    }
}

static void fillRawLocals(QTreeWidget* locals, Lua::Engine2* lua)
{
    Lua::Engine2::LocalVars vs = lua->getLocalVars(true,2,55,true);
    foreach( const Lua::Engine2::LocalVar& v, vs )
    {
        QTreeWidgetItem* item = new QTreeWidgetItem(locals);
        QString name = v.d_name;
        if( v.d_isUv )
            name = "(" + name + ")";
        item->setText(0,name);
        if( v.d_value.canConvert<Lua::Engine2::VarAddress>() )
        {
            if( v.d_type == Lua::Engine2::LocalVar::CDATA )
            {
                Lua::Engine2::VarAddress addr = v.d_value.value<Lua::Engine2::VarAddress>();
                char str[40];
                memcpy(str,addr.d_addr,39);
                str[39] = 0;
                item->setText(1, QString("\"%1\"").arg(str).simplified());
            }else
                typeAddr(item,v.d_value);
        }else if( v.d_value.type() == QMetaType::QVariantMap )
        {
            typeAddr(item,v.d_value);
            fillLocalSubs(item,v.d_value.toMap() );
        }else if( Lua::JitBytecode::isString(v.d_value) )
        {
            item->setText(1, "\"" + v.d_value.toString().simplified() + "\"");
            item->setToolTip(1, v.d_value.toString() );
        }else if( !v.d_value.isNull() )
            item->setText(1,v.d_value.toString());
        else
        {
            switch( v.d_type )
            {
            case Lua::Engine2::LocalVar::NIL:
                item->setText(1, "nil");
                break;
            case Lua::Engine2::LocalVar::FUNC:
                item->setText(1, "func");
                break;
            case Lua::Engine2::LocalVar::TABLE:
                item->setText(1, "table");
                break;
            case Lua::Engine2::LocalVar::STRUCT:
                item->setText(1, "struct");
                break;
            case Lua::Engine2::LocalVar::STRING:
                item->setText(1, "\"" + v.d_value.toString().simplified() + "\"");
                break;
            default:
                break;
           }
        }
    }
}

void Ide::fillLocals()
{
    d_locals->clear();

    if( d_rt->getLua()->getMode() == Lua::Engine2::PcMode )
    {
        fillRawLocals(d_locals, d_rt->getLua());
        d_locals->parentWidget()->show();
        return;
    }

    lua_Debug ar;
    const int level = d_rt->getLua()->getActiveLevel();
    Declaration* scope = d_scopes[ level ];
    if( scope && lua_getstack( d_rt->getLua()->getCtx(), level, &ar ) )
    {
        Declaration* cur = scope->link;
        while( cur )
        {
            if( cur->kind == Declaration::ParamDecl || cur->kind == Declaration::LocalDecl )
            {
                QTreeWidgetItem* item = new QTreeWidgetItem(d_locals);
                const int before = lua_gettop(d_rt->getLua()->getCtx());
                if( lua_getlocal( d_rt->getLua()->getCtx(), &ar, cur->id + 1 ) )
                {
                    item->setText(0,cur->name);
                    printLocalVal(item,cur->type, 0);
                    lua_pop( d_rt->getLua()->getCtx(), 1 );
                }else
                    item->setText(0,"<invalid>");
                Q_ASSERT( before == lua_gettop(d_rt->getLua()->getCtx()) );
            }
            cur = cur->getNext();
        }
#if 1
        Declaration* m = scope->getModule();
        QTreeWidgetItem* parent = new QTreeWidgetItem(d_locals);
        QString name = m->getModuleFullName(true);
        parent->setToolTip(0,name);
        if( name.size() > 20 )
            name = name.left(20) + "...";
        parent->setText(0,name);
        parent->setText(1,"<module>");
        const int before = lua_gettop(d_rt->getLua()->getCtx());
        lua_getglobal( d_rt->getLua()->getCtx(), m->getModuleFullName() );
        if( !lua_isnil( d_rt->getLua()->getCtx(), -1 ) )
        {
            const int mod = lua_gettop( d_rt->getLua()->getCtx() );
            cur = m->link;
            while(cur)
            {
                if( cur->kind == Declaration::VarDecl )
                {
                    QTreeWidgetItem* item = new QTreeWidgetItem(parent);
                    item->setText(0,cur->name);
                    const int before = lua_gettop(d_rt->getLua()->getCtx());
                    lua_rawgeti( d_rt->getLua()->getCtx(), mod, cur->id );
                    printLocalVal(item,cur->type, 0);
                    lua_pop( d_rt->getLua()->getCtx(), 1 );
                    Q_ASSERT( before == lua_gettop(d_rt->getLua()->getCtx()) );
                }
                cur = cur->getNext();
            }
        }else
            parent->setText(1,"??");
        lua_pop( d_rt->getLua()->getCtx(), 1 ); // module
        Q_ASSERT( before == lua_gettop(d_rt->getLua()->getCtx()) );
#endif
    }
#if 0 // TEST, usually 0
    fillRawLocals(d_locals, d_rt->getLua());
#endif
    d_locals->parentWidget()->show();
}

static inline Type* derefed( Type* type )
{
    if( type == 0 )
        return 0;
    return type->deref();
}

template <class T>
static inline void createArrayElems( QTreeWidgetItem* parent, const void* ptr,
                                     int bytecount, int numOfFetchedElems, char format = 0 )
{
    const int count = bytecount / sizeof(T);
    parent->setText(1, QString("<array length %1>").arg(count) );
    const T* arr = (const T*)ptr;
    for( int i = 0; i < qMin(count,numOfFetchedElems); i++ )
    {
        QTreeWidgetItem* item = new QTreeWidgetItem(parent);
        item->setText(0,QString::number(i));
        switch(format)
        {
        case 'b':
            item->setText(1, arr[i] ? "true" : "false" );
            break;
        case 's':
            item->setText(1,QString("{%1}").arg((quint32)arr[i],32,2,QChar('0')));
            break;
        default:
            item->setText(1,QString::number(arr[i]));
            break;
        }
    }
}

static QString nameOf( Type* r, bool frame = false )
{
    QString name;
    Declaration* decl = r->decl;
    if( decl )
        name = decl->scopedName(true);
    else if( frame )
        name = "<record>";
    else
        name = "record";
    return name;
}

static Type* lookupType(Declaration* scope, const QByteArrayList& path)
{
    int i = 0;
    while( i < path.size() && scope )
    {
        const QByteArray name = Token::getSymbol(path[i]);
        scope = scope->find(name,false);
        i++;
    }
    if( scope && scope->kind == Declaration::TypeDecl )
        return scope->type;
    else
        return 0;
}

void Ide::printLocalVal(QTreeWidgetItem* item, Type* type, int depth)
{
    static const int numOfFetchedElems = 100;
    static const int numOfLevels = 7;

    if( depth > numOfLevels )
        return;
    lua_State* L = d_rt->getLua()->getCtx();
    type = derefed(type);
    Q_ASSERT( type );
    if( type->form < BasicType::Max )
    {
        switch( type->form )
        {
        case BasicType::BOOLEAN:
            if( lua_toboolean(L, -1) )
                item->setText(1,"true");
            else
                item->setText(1,"false");
            return;
        case BasicType::CHAR:
            {
                const ushort uc = lua_tointeger(L,-1);
                const QString ch = QChar( uc );
                item->setText(1,QString("'%1' %2x").arg(ch.simplified()).arg(uc,0,16));
            }
            return;
        case BasicType::INTEGER:
        case BasicType::BYTE:
            item->setText(1,QString::number(lua_tointeger(L,-1)));
            return;
        case BasicType::REAL:
            item->setText(1,QString::number(lua_tonumber(L,-1)));
            return;
        case BasicType::SET:
            item->setText(1,QString("{%1}").arg((quint32)lua_tointeger(L,-1),32,2,QChar('0')));
            return;
        case BasicType::Nil:
            item->setText(1,"nil");
            return;
        case BasicType::StrLit:
        case BasicType::STRING:
            item->setText(1,QString("\"%1\"").arg(lua_tostring(L,-1)));
            return;
        case BasicType::ByteArrayLit:
            item->setText(1,QString("$%1$").arg(QByteArray(lua_tostring(L,-1)).toHex().constData()));
            return;
        }
    } // else
    switch( type->form )
    {
    case Type::Array:
        {
            Type* a = type;
            Type* at = derefed(a->base);
            Q_ASSERT( at );
            const int arr = lua_gettop(L);
            const int luatype = lua_type(L, arr );
            if( lua_isnil(L, -1) )
                item->setText(1, QString("nil") );
            else if( luatype == 10 ) // cdata
            {
                const void* ptr = lua_topointer(L, -1);
                QString str;
                if( at->form == BasicType::CHAR )
                {
                    const quint8* buf = (const quint8*)ptr;
                    for(int i = 0; i < numOfFetchedElems; i++ )
                    {
                        quint8 ch = buf[i];
                        if( ch == 0 )
                            break;
                        str += QChar((ushort) ch );
                    }
                    item->setText(1,QString("\"%1\"").arg(str));
                }else
                {
                    lua_getglobal(L, "LUON");
                    lua_rawgeti(L, -1, 26 ); // bytesize
                    lua_pushvalue( L, arr );
                    lua_pcall( L, 1, 1, 0 );
                    const int bytesize = lua_tointeger( L, -1 );
                    lua_pop( L, 2 ); // LUON, count
                    switch( at->form )
                    {
                    case BasicType::BOOLEAN:
                        createArrayElems<quint8>( item, ptr, bytesize, numOfFetchedElems, 'b');
                        break;
                    case BasicType::BYTE:
                        createArrayElems<quint8>( item, ptr, bytesize, numOfFetchedElems);
                        break;
                    case BasicType::INTEGER:
                        createArrayElems<qint32>( item, ptr, bytesize, numOfFetchedElems);
                        break;
                    case BasicType::REAL:
                        createArrayElems<float>( item, ptr, bytesize, numOfFetchedElems);
                        break;
                    case BasicType::SET:
                        createArrayElems<quint32>( item, ptr, bytesize, numOfFetchedElems, 's');
                        break;
                    case BasicType::Nil:
                    case BasicType::StrLit:
                    case BasicType::ByteArrayLit:
                        Q_ASSERT( false );
                        break;
                    }
                }
            }else if( luatype != LUA_TTABLE )
            {
                item->setText(1, QString("<invalid array> %1").arg(lua_tostring(L, arr) ) );
            }else
            {
                lua_getfield( L, arr, "count" );
                const int count = lua_tointeger( L, -1 );
                lua_pop( L, 1 );
                item->setText(1, QString("<array length %1>").arg(count) );
                for( int i = 0; i < qMin(count,numOfFetchedElems); i++ )
                {
                    QTreeWidgetItem* sub = new QTreeWidgetItem(item);
                    sub->setText(0,QString::number(i));
                    lua_rawgeti( L, arr, i );
                    printLocalVal(sub,at,depth+1);
                    lua_pop( L, 1 );
                }
            }
        }
        break;
    case Type::ConstEnum:
        {
            const int e = lua_tointeger(L,-1);
            Type* et = type;
            if( e >= 0 && e < et->subs.size() )
                item->setText(1,et->subs[e]->name);
            else
                item->setText(1,"<invalid enum value>");
        }
        break;
    case Type::Proc:
        if( lua_isnil( L, -1 ) )
            item->setText(1,"nil");
        else
        {
            const void* ptr = lua_topointer(L, -1);
#if Q_PROCESSOR_WORDSIZE == 4
            const quint32 ptr2 = (quint32)ptr;
#else
            const quint64 ptr2 = (quint64)ptr;
#endif
            item->setText(1,QString("<proc 0x%1>").arg(ptr2,0,16));
        }
        break;
    case Type::Record:
        {
            const int rec = lua_gettop(L);
            Type* r = type;
            const int luaType = lua_type( L, rec );
            if( lua_isnil(L, rec) )
            {
                item->setText(1,"nil");
                break;
            }else if( luaType == 10 ) // ffi type
            {
                item->setText(1,nameOf(r,true));
                foreach( Declaration* f, r->subs )
                {
                    QTreeWidgetItem* sub = new QTreeWidgetItem(item);
                    sub->setText(0,f->name);
                    lua_getfield( L, rec, f->name );
                    printLocalVal(sub,derefed(f->type),depth+1);
                    lua_pop( L, 1 );
                }
                break;
            }else if( luaType != LUA_TTABLE )
            {
                item->setText(1,tr("<invalid class %1, lua type %2>").arg(nameOf(r)).arg(luaType));
                // qWarning() << "wrong type, expecting table, got type" << type;
                // happens when a procedure is entered before initialization code could run
                break;
            }

            QByteArray className;
            // look for the dynamic type
            if( lua_getmetatable(L,rec) )
            {
                lua_getfield(L, -1, "@name");
                if( !lua_isnil( L, -1 ) )
                    className = lua_tostring(L, -1);
                lua_pop(L,1); // field
                lua_pop(L,1); // metatable

                // show dynamic instead of static type
                const QByteArrayList path = className.split('.');
                Declaration* m = d_rt->getPro()->findModule(path.first());
                if( m )
                {
                    const QByteArrayList path2 = path.last().split('$');
                    Type* t = lookupType(m, path2);
                    if( t )
                        t = t->deref();
                    if( t && t->form == Type::Record )
                        r = t;
                }
            }
            if( className.isEmpty() )
                className = QString("<no class, should be %1>").arg(nameOf(r,true)).toUtf8();
            else
                className.replace('/','.');
            item->setText(1,className);
            QList<Declaration*> fields = r->fieldList();
            foreach( Declaration* f, fields )
            {
                if( f->kind != Declaration::Field )
                    continue;
                QTreeWidgetItem* sub = new QTreeWidgetItem(item);
                sub->setText(0,f->name);
                lua_rawgeti( L, rec, f->id );
                printLocalVal(sub,derefed(f->type),depth+1);
                lua_pop( L, 1 );
            }
        }
        break;
    default:
        item->setText(1,lua_tostring(L,-1));
        break;
    }
}

static void fillModItems(QTreeWidgetItem* item, Declaration* n, Declaration* p, Type* r,
                         bool sort, QHash<Declaration*,QTreeWidgetItem*>& idx , Project* pro);

static void fillRecord(QTreeWidgetItem* item, Declaration* n, Type* r, bool sort,
                       QHash<Declaration*,QTreeWidgetItem*>& idx, Project* pro )
{
    fillModItems(item,n, 0, r, sort, idx, pro);
    if( r->base )
        item->setText(0, item->text(0) + " ⇑");
    if( n->hasSubs )
    {
        DeclList subs = pro->getSubs(n);
        item->setText(0, item->text(0) + QString(" ⇓%1").arg(subs.size()));
    }
    item->setToolTip( 0, item->text(0) );
}

template<class T>
static void createModItem(T* parent, Declaration* n, Type* t, bool nonbound, bool sort,
                          QHash<Declaration*,QTreeWidgetItem*>& idx, Project* pro )
{
    bool isAlias = false;
    if( t == 0 )
        t = n->type;
    else
        isAlias = true;
    if( t == 0 )
        return;
    if( idx.contains(n) )
    {
        // qWarning() << "fillMod recursion at" << n->getModule()->d_file << n->pos.d_row << n->name;
        return; // can legally happen if record decl contains a typedef using record, as e.g. Meta.Item.ParamCallVal.IP
    }
    switch( n->kind )
    {
    case Declaration::TypeDecl:
        switch( t->form )
        {
        case Type::Record:
            {
                QTreeWidgetItem* item = new QTreeWidgetItem(parent);
                if( !isAlias  )
                    fillRecord(item,n,t,sort,idx, pro);
                else
                    fillModItems(item,n, 0, 0, sort, idx, pro);
            }
            break;
        case Type::NameRef:
            if( t->deref()->form == Type::Record )
                createModItem(parent,n,t->deref(),nonbound, sort, idx, pro);
            break;
        }
        break;
    case Declaration::Procedure:
        if( !nonbound || n->mode != Declaration::Receiver )
        {
            QTreeWidgetItem* item = new QTreeWidgetItem(parent);
            fillModItems(item,n, n, 0, sort, idx, pro);
            if( n->super )
                item->setText(0, item->text(0) + " ⇑");
            if( n->hasSubs )
            {
                DeclList subs = pro->getSubs(n);
                item->setText(0, item->text(0) + QString(" ⇓%1").arg(subs.size()));
            }
            item->setToolTip( 0, item->text(0) );
        }
        break;
    }
}

template <class T>
static void walkModItems(T* parent, Declaration* p, Type* r, bool sort,
                         QHash<Declaration*,QTreeWidgetItem*>& idx, Project* pro)
{
    typedef QMultiMap<QByteArray,Declaration*> Sort;
    if( p && sort)
    {
        Sort tmp;
        Declaration* n = p->link;
        while( n )
        {
            tmp.insert( n->name.toLower(), n );
            n = n->getNext();
        }
        Sort::const_iterator i;
        for( i = tmp.begin(); i != tmp.end(); ++i )
            createModItem(parent,i.value(),0,true, sort, idx, pro);
    }else if( p )
    {
        Declaration* n = p->link;
        while( n )
        {
            createModItem(parent,n,0,true, sort, idx, pro);
            n = n->getNext();
        }
    }
    if( r && sort )
    {
        Sort tmp;
        foreach( Declaration* n, r->subs )
        {
            if( n->kind == Declaration::Procedure )
                tmp.insert( n->name.toLower(), n );
        }
        Sort::const_iterator i;
        for( i = tmp.begin(); i != tmp.end(); ++i )
            createModItem(parent,i.value(),0,false, sort, idx, pro);
    }else if( r )
    {
        foreach( Declaration* n, r->subs )
        {
            if( n->kind == Declaration::Procedure )
                createModItem(parent,n,0,false, sort, idx, pro);
        }
    }
}

static void fillModItems( QTreeWidgetItem* item, Declaration* n, Declaration* p, Type* r,
                          bool sort, QHash<Declaration*,QTreeWidgetItem*>& idx, Project* pro )
{
    const bool pub = n->visi >= Declaration::Private;
    item->setText(0,n->name);
    item->setData(0, Qt::UserRole, QVariant::fromValue(n) );
    idx.insert(n,item);
    switch( n->kind )
    {
    case Declaration::TypeDecl:
        if( r && r->base == 0 && r->subs.isEmpty() )
            item->setIcon(0, QPixmap( pub ? ":/images/struct.png" : ":/images/struct_priv.png" ) );
        else if( r == 0 && p == 0 )
            item->setIcon(0, QPixmap( pub ? ":/images/alias.png" : ":/images/alias_priv.png" ) );
        else
            item->setIcon(0, QPixmap( pub ? ":/images/class.png" : ":/images/class_priv.png" ) );
        break;
    case Declaration::Procedure:
        item->setIcon(0, QPixmap( pub ? ":/images/func.png" : ":/images/func_priv.png" ) );
        break;
    }
    walkModItems(item,p,r,sort, idx, pro);
}

void Ide::fillModule(Declaration* m)
{
    d_mod->clear();
    d_modIdx.clear();
    d_modTitle->clear();
    if( m == 0 )
        return;
    d_modTitle->setText( QString("'%1'").arg(m->name.constData()) );
    walkModItems(d_mod, m, 0, true, d_modIdx, d_rt->getPro() );
}

template<class T>
static QTreeWidgetItem* fillHierProc( T* parent, Declaration* p, Declaration* ref, Project* pro )
{
    QTreeWidgetItem* item = new QTreeWidgetItem(parent);
    Q_ASSERT( p->mode == Declaration::Receiver && p->link && p->link->mode == Declaration::Receiver && p->link->type );

    Qualident record = p->link->type->decl->data.value<Qualident>();
    item->setText(0, record.second);
    item->setData(0, Qt::UserRole, QVariant::fromValue(p) );
    item->setIcon(0, QPixmap( p->visi >= Declaration::ReadWrite ? ":/images/func.png" : ":/images/func_priv.png" ) );
    item->setToolTip(0,item->text(0));

    QTreeWidgetItem* ret = 0;
    DeclList subs = pro->getSubs(p);
    foreach( Declaration* sub, subs )
    {
        QTreeWidgetItem* tmp = fillHierProc(item, sub, ref, pro);
        if( tmp )
            ret = tmp;
    }
    if( ret == 0 && p == ref )
            ret = item;
    return ret;
}

template<class T>
static QTreeWidgetItem* fillHierClass( T* parent, Declaration* n, Type* p, Type* ref, Project* pro )
{
    QTreeWidgetItem* item = new QTreeWidgetItem(parent);
    Declaration* name = p->decl;
    if( name )
#if 0
        // TODO
        item->setText(0, name->getQualifiedName().join('.') );
    else
#endif
        item->setText(0, name->name);
    item->setData(0, Qt::UserRole, QVariant::fromValue( name ) );
    item->setIcon(0, QPixmap( name->visi >= Declaration::ReadWrite ? ":/images/class.png" : ":/images/class_priv.png" ) );
    item->setToolTip(0,item->text(0));
    QTreeWidgetItem* ret = 0;
    DeclList subs = pro->getSubs(n);
    foreach( Declaration* sub, subs )
    {
        QTreeWidgetItem* tmp = fillHierClass(item, sub, sub->type, ref, pro);
        if( tmp )
            ret = tmp;
    }
    if( ret == 0 )
            ret = item;
    return ret;
}
void Ide::fillHier(Declaration* n)
{
    if( d_lock4 )
        return;
    d_hier->clear();
    d_hierTitle->clear();
    if( n == 0 )
        return;
    QFont f = d_hier->font();
    f.setBold(true);
    QTreeWidgetItem* ref = 0;
    switch( n->kind )
    {
    case Declaration::TypeDecl:
        switch( n->type->form )
        {
        case Type::Record:
            {
                Type* r = n->type;
                Type* r0 = r;
                d_hierTitle->setText( QString("Inheritance of class '%1'").arg( n->name.constData() ) );
                while( r->base )
                    r = r->base;
                ref = fillHierClass( d_hier, n, r, r0, d_rt->getPro() );
                Q_ASSERT( ref );
            }
            break;
        }
        break;
    case Declaration::Procedure:
        {
            Declaration* p = n;
            if( p->mode != Declaration::Receiver )
                return;
            d_hierTitle->setText( QString("Overrides of procedure '%1'").arg( n->name.constData() ) );
            while( p->super )
                p = p->super;
            ref = fillHierProc( d_hier, p, n, d_rt->getPro() );
            Q_ASSERT( ref );
        }
        break;
    }
    d_hier->sortByColumn(0,Qt::AscendingOrder);
    if( ref )
    {
        ref->setFont(0,f);
        // d_hier->expandAll();
        ref->setExpanded(true);
        d_hier->scrollToItem(ref,QAbstractItemView::PositionAtCenter);
        d_hier->setCurrentItem(ref);
    }
}

void Ide::removePosMarkers()
{
    for( int i = 0; i < d_tab->count(); i++ )
    {
        Editor* e = static_cast<Editor*>( d_tab->widget(i) );
        e->setPositionMarker(-1);
    }
    d_bcv->clearMarker();
}

void Ide::enableDbgMenu()
{
    d_dbgBreak->setEnabled(!d_rt->getLua()->isWaiting() && d_rt->getLua()->isExecuting() && d_rt->getLua()->isDebug() );
    d_dbgAbort->setEnabled(d_rt->getLua()->isWaiting());
    d_dbgContinue->setEnabled(d_rt->getLua()->isWaiting());
    d_dbgStepIn->setEnabled(d_rt->getLua()->isWaiting() && d_rt->getLua()->isDebug() );
    d_dbgStepOver->setEnabled(d_rt->getLua()->isWaiting() && d_rt->getLua()->isDebug() );
    d_dbgStepOut->setEnabled(d_rt->getLua()->isWaiting() && d_rt->getLua()->isDebug() );
}

void Ide::handleGoBack()
{
    ENABLED_IF( d_backHisto.size() > 1 );

    d_pushBackLock = true;
    d_forwardHisto.push_back( d_backHisto.last() );
    d_backHisto.pop_back();
    showEditor( d_backHisto.last() );
    d_pushBackLock = false;
}

void Ide::handleGoForward()
{
    ENABLED_IF( !d_forwardHisto.isEmpty() );

    Location cur = d_forwardHisto.last();
    d_forwardHisto.pop_back();
    showEditor( cur );
}

void Ide::onUpdateLocation(int line, int col)
{
    Editor* e = static_cast<Editor*>( sender() );
    e->clearBackHisto();
    pushLocation(Location(e->getPath(), line,col,e->verticalScrollBar()->value()));
}

void Ide::onXrefDblClicked()
{
    QTreeWidgetItem* item = d_xref->currentItem();
    if( item )
    {
        Symbol* sym = item->data(0,Qt::UserRole).value<Symbol*>();
        Declaration* module = item->data(1,Qt::UserRole).value<Declaration*>();
        Q_ASSERT( sym != 0 );
        d_lock3 = true;
        if( module )
        {
            d_curModule = module;
            ModuleData md = module->data.value<ModuleData>();
            showEditor( md.source, sym->pos.d_row, sym->pos.d_col, false, true );
        }
        d_lock3 = false;
    }
}

void Ide::onToggleBreakPt()
{
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    ENABLED_IF( edit );

    quint32 line;
    const bool on = edit->toggleBreakPoint(&line);
    Project::File* f = d_rt->getPro()->findFile(edit->getPath());
    Q_ASSERT( f && f->d_mod );
    if( on )
        d_rt->getLua()->addBreak( f->d_mod->name, line + 1 );
    else
        d_rt->getLua()->removeBreak( f->d_mod->name, line + 1 );
}

void Ide::onSingleStep()
{
    // ENABLED_IF( d_rt->getLua()->isWaiting() );

    d_rt->getLua()->runToNextLine();
}

void Ide::onStepOver()
{
    d_rt->getLua()->runToNextLine(Lua::Engine2::StepOver);
}

void Ide::onStepOut()
{
    d_rt->getLua()->runToNextLine(Lua::Engine2::StepOut);
}

void Ide::onContinue()
{
    // ENABLED_IF( d_rt->getLua()->isWaiting() );

    d_rt->getLua()->runToBreakPoint();
}

void Ide::onShowLlBc()
{
    ENABLED_IF( d_bcv->topLevelItemCount() );

    Lua::BcViewer* bc = new Lua::BcViewer();
    QBuffer buf( &d_curBc );
    buf.open(QIODevice::ReadOnly);
    bc->loadFrom( &buf );
    bc->show();
    bc->setAttribute(Qt::WA_DeleteOnClose);
}

void Ide::onWorkingDir()
{
    ENABLED_IF(true);

    bool ok;
    const QString res = QInputDialog::getText(this,tr("Oberon File System Root"),
                                              tr("Enter Path (supports %PRODIR% and %APPDIR%):"), QLineEdit::Normal,
                                              d_rt->getPro()->getWorkingDir(), &ok );
    if( !ok )
        return;
    d_rt->getPro()->setWorkingDir(res);
}

void Ide::onLuaNotify(int messageType, QByteArray val1, int val2)
{
    switch( messageType )
    {
    case Lua::Engine2::Started:
    case Lua::Engine2::Continued:
    case Lua::Engine2::LineHit:
    case Lua::Engine2::BreakHit:
    case Lua::Engine2::ErrorHit:
    case Lua::Engine2::Finished:
    case Lua::Engine2::Aborted:
        enableDbgMenu();
        break;
    }
}

void Ide::pushLocation(const Ide::Location& loc)
{
    if( d_pushBackLock )
        return;
    if( !d_backHisto.isEmpty() && d_backHisto.last() == loc )
        return; // o ist bereits oberstes Element auf dem Stack.
    d_backHisto.removeAll( loc );
    d_backHisto.push_back( loc );
}

void Ide::clear()
{
    d_backHisto.clear();
    d_forwardHisto.clear();
    d_rt->getPro()->clear();
    d_mods->clear();
    d_mod->clear();
    d_hier->clear();
    d_modIdx.clear();
    d_stack->clear();
    d_scopes.clear();
    d_locals->clear();
    d_xrefTitle->clear();
    d_modTitle->clear();
    d_hierTitle->clear();
    d_xref->clear();
    d_errs->clear();
}

void Ide::showBc(const QByteArray& bc)
{
    if( !bc.isEmpty() )
    {
        d_curBc = bc;
        QBuffer buf( &d_curBc );
        buf.open(QIODevice::ReadOnly);
        d_bcv->loadFrom(&buf);
    }else
        d_bcv->clear();
}

void Ide::onAbout()
{
    ENABLED_IF(true);

    QMessageBox::about( this, qApp->applicationName(),
      tr("<html>Release: %1   Date: %2<br><br>"

      "Welcome to the Luon IDE.<br>"
      "See <a href=\"https://github.com/rochus-keller/Luon\">"
         "here</a> for more information.<br><br>"

      "Author: Rochus Keller, me@rochus-keller.ch<br><br>"

      "Licese: <a href=\"https://www.gnu.org/licenses/license-list.html#GNUGPL\">GNU GPL v2 or v3</a>"
      "</html>" ).arg( qApp->applicationVersion() ).arg( QDateTime::currentDateTime().toString("yyyy-MM-dd") ));
}

void Ide::onQt()
{
    ENABLED_IF(true);
    QMessageBox::aboutQt(this,tr("About the Qt Framework") );
}

void Ide::onExpMod()
{
    ENABLED_IF( d_mods->currentItem() );

    Declaration* m = d_mods->currentItem()->data(0,Qt::UserRole).value<Declaration*>();
    if( m == 0 || m->kind != Declaration::Module )
        return;

    const QString path = QFileDialog::getSaveFileName( this, tr("Export Module"), m->getModuleFullName(true) + ".luon" );

    if( path.isEmpty() )
        return;
    ModuleData md = m->data.value<ModuleData>();
    d_rt->getPro()->printTreeShaken( md.source, path );
}

void Ide::onBcDebug()
{
    CHECKED_IF( true, d_rt->getLua()->getMode() == Lua::Engine2::PcMode );

    if( d_rt->getLua()->getMode() == Lua::Engine2::PcMode )
        d_rt->getLua()->setDebugMode( Lua::Engine2::LineMode );
    else
        d_rt->getLua()->setDebugMode( Lua::Engine2::PcMode );
}

void Ide::onRowColMode()
{
    CHECKED_IF( true, d_rt->getLua()->getMode() == Lua::Engine2::RowColMode );

    if( d_rt->getLua()->getMode() == Lua::Engine2::RowColMode )
        d_rt->getLua()->setDebugMode( Lua::Engine2::LineMode );
    else
        d_rt->getLua()->setDebugMode( Lua::Engine2::RowColMode );
}

void Ide::onShowBcFile()
{
    ENABLED_IF(true);

    const QString path = QFileDialog::getOpenFileName(this,tr("Open bytecode file"),QString());

    if( path.isEmpty() )
        return;

    QDir::setCurrent(QFileInfo(path).absolutePath());


    Lua::BcViewer2* view = new Lua::BcViewer2();
    view->loadFrom(path);
    view->setWindowTitle( tr("%1 - Bytecode View").arg( QFileInfo(path).fileName() ) );
    view->show();
}

void Ide::onJitEnabled()
{
    CHECKED_IF(true,d_rt->jitEnabled());

    d_rt->setJitEnabled( !d_rt->jitEnabled() );
}

void Ide::onRestartLua()
{
    ENABLED_IF( !d_rt->getLua()->isExecuting() );

    d_rt->restartEngine();
}

void Ide::onRunCommand()
{
    ENABLED_IF(true);

    bool ok = false;
    QString res = QInputDialog::getText(this,tr("Set Command"),tr("<module>[.<command>] to run at start of program:"),
                             QLineEdit::Normal, d_rt->getPro()->renderMain(), &ok );
    if( !ok )
        return;

    Project::ModProc m;
    QStringList tmp = res.split('.');
    if( tmp.size() > 2 || ( tmp.first().isEmpty() && !tmp.last().isEmpty() ) )
        QMessageBox::critical(this,tr("Set Command"), tr("Invalid command syntax") );
    else
    {
        m.first = tmp.first().toUtf8();
        if( tmp.size() == 2 )
            m.second = tmp.last().toUtf8();
        d_rt->getPro()->setMain(m);
    }
}

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    a.setOrganizationName("me@rochus-keller.ch");
    a.setOrganizationDomain("github.com/rochus-keller/Luon");
    a.setApplicationName("Luon IDE (LuaJIT)");
    a.setApplicationVersion("0.7.6");
    a.setStyle("Fusion");    
    QFontDatabase::addApplicationFont(":/font/DejaVuSansMono.ttf"); // "DejaVu Sans Mono"

    Ide w;
    if( a.arguments().size() > 1 )
        w.loadFile(a.arguments()[1] );

    return a.exec();
}
