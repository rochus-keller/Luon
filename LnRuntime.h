#ifndef LNRUNTIME_H
#define LNRUNTIME_H

#include <QObject>


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

namespace Ln
{

    class Runtime : public QObject
    {
        Q_OBJECT
    public:
        Runtime(const QString& path);

    public slots:
        void run();

    signals:
        void finished();

    protected slots:
        void onNotify(int, const QByteArray &, int);

    private:
        QString path;
    };

}

#endif // LNRUNTIME_H
