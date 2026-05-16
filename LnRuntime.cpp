/*
* Copyright 2026 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include "LnRuntime.h"
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
#include <QApplication>
#include <QFileInfo>
#include <QDir>
#include <QTimer>
using namespace Ln;

Runtime::Runtime(const QString &path):path(path)
{

}

void Runtime::run()
{
    LjRuntime rt;
    rt.getPro()->loadFrom(path);
    rt.getLua()->setPrintToStdout(true);
    connect( rt.getLua(), SIGNAL(onNotify(int,QByteArray,int)), this, SLOT(onNotify(int,QByteArray,int)) );

    QDir::setCurrent(QFileInfo(path).absolutePath());
    rt.run();
    foreach(const QByteArray& res, rt.getLua()->getReturns() )
        qDebug() << res.constData();
    emit finished();
}

void Runtime::onNotify(int messageType, const QByteArray& val1, int val2)
{
    switch( messageType )
    {
    case Lua::Engine2::Print:
        qDebug() << val1.constData();
        break;
    case Lua::Engine2::Error:
        qCritical() << val1.constData();
        break;
    case Lua::Engine2::Cout:
        fprintf(stdout,"%s", val1.constData());
        fflush(stdout);
        break;
    case Lua::Engine2::Cerr:
        fprintf(stderr,"%s", val1.constData());
        fflush(stderr);
        break;
    case Lua::Engine2::Aborted:
    case Lua::Engine2::Finished:
        break;
    default:
        break;
    }
    QApplication::processEvents();
}

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    a.setOrganizationName("me@rochus-keller.ch");
    a.setOrganizationDomain("github.com/rochus-keller/Luon");
    a.setApplicationName("Luon Runtime (LuaJIT)");
    a.setApplicationVersion("0.1.0");
    a.setStyle("Fusion");


    QString path;
    if( a.arguments().size() > 1 )
        path = a.arguments()[1];
    else
    {
        qCritical() << "expecting the path to a project file";
        return 0;
    }

    Runtime rt(path);

    QObject::connect(&rt, SIGNAL(finished()), &a, SLOT(quit()), Qt::QueuedConnection);
    // Schedule run() to be called as soon as the event loop starts
    QTimer::singleShot(0, &rt, SLOT(run()));

    return a.exec();
}
