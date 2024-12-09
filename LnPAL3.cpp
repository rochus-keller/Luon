/*
* Copyright 2021-2024 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Luon runtime library.
*
* The following is the license that applies to this copy of the
* file. For a license to use the file under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* This file may be used under the terms of the GNU Lesser
* General Public License version 2.1 or version 3 as published by the Free
* Software Foundation and appearing in the file LICENSE.LGPLv21 and
* LICENSE.LGPLv3 included in the packaging of this file. Please review the
* following information to ensure the GNU Lesser General Public License
* requirements will be met: https://www.gnu.org/licenses/lgpl.html and
* http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
*/
// Adopted from Smalltalk StDisplay.cpp

#include "LnPAL3.h"
#include <QFile>
#include <QPainter>
#include <stdint.h>
#include <QtDebug>
#include <QBitmap>
#include <QMessageBox>
#include <QApplication>
#include <QCloseEvent>
#include <QShortcut>
#include <QClipboard>
#include <bitset>
using namespace St;

#define _USE_BB_IMP_

static Display* s_inst = 0;
bool Display::s_run = true;
bool Display::s_break = false;
bool Display::s_copy = false;
QList<QFile*> Display::s_files;

static const int s_msPerFrame = 30; // 20ms according to BB
enum { whitePixel = 1, blackPixel = 0 };
static QFile s_out("st.log");

extern "C" { uint32_t PAL2_getTime(); }

Display::Display(QWidget *parent) : QWidget(parent),d_curX(-1),d_curY(-1),d_capsLockDown(false),
    d_shiftDown(false),d_recOn(false),d_forceClose(false)
{
    setMouseTracking(true);
    setFocusPolicy(Qt::StrongFocus);
    setCursor(Qt::BlankCursor);
    setWindowTitle( renderTitle() );
    show();
    d_lastEvent = 0;
    new QShortcut(tr("ALT+R"), this, SLOT(onRecord()) );
    new QShortcut(tr("ALT+L"), this, SLOT(onLog()) );
    new QShortcut(tr("ALT+X"), this, SLOT(onExit()) );
    new QShortcut(tr("ALT+B"), this, SLOT(onBreak()) );
    new QShortcut(tr("ALT+V"), this, SLOT(onPaste()) );
    new QShortcut(tr("ALT+SHIFT+V"), this, SLOT(onPasteBenchmark()) );
    new QShortcut(tr("ALT+C"), this, SLOT(onCopy()) );
}

Display::~Display()
{
    s_inst = 0;
    foreach( QFile* f, s_files )
        delete f;
    s_files.clear();
}

Display*Display::inst()
{
    if( s_inst == 0 )
    {
        s_inst = new Display();
        s_run = true;
    }
    return s_inst;
}

void Display::forceClose()
{
    if( s_inst )
    {
        s_inst->d_forceClose = true;
        s_inst->close();
    }
}

void Display::setBitmap(const Bitmap& buf)
{
    d_bitmap = buf;
    d_screen = QImage( buf.width(), buf.height(), QImage::Format_RGB32 );
    d_bitmap.toImage(d_screen);
    d_updateArea = QRect();
    setFixedSize( buf.width(), buf.height() );
    update();
}

void Display::setCursorBitmap(const Bitmap& bm)
{
    QImage cursor( bm.width(), bm.height(), QImage::Format_RGB32 );
    bm.toImage(cursor);
    QBitmap pix = QPixmap::fromImage( cursor );
    setCursor( QCursor( pix, pix, 0, 0 ) );
    // update();
}

void Display::setCursorPos(qint16 x, qint16 y)
{
    d_curX = x;
    d_curY = y;
    update();
}

void Display::drawRecord(int x, int y, int w, int h)
{
    if( !d_recOn )
        return;
    QPainter p(&d_record);
    if( w < 0 || h < 0 )
        p.setPen(Qt::red);
    else
        p.setPen(Qt::green);
    p.drawRect(x,y,w,h);
}

void Display::updateArea(const QRect& r )
{
    d_updateArea |= r;

    update( r );
}

void Display::setLog(bool on)
{
    if( on && !s_out.isOpen() )
        onLog();
    else if( !on && s_out.isOpen() )
        onLog();
}

void Display::processEvents()
{
    static quint32 last = 0;
    static quint32 count = 0;

    if( count > 4000 )
    {
        count = 0;
        Display* d = Display::inst();
        const quint32 cur = PAL2_getTime();
        if( ( cur - last ) >= 30 )
        {
            last = cur;
            QApplication::processEvents();
        }
    }else
        count++;
}

void Display::copyToClipboard(const QByteArray& str)
{
    QString text = QString::fromUtf8(str);
    text.replace( '\r', '\n' );
    QApplication::clipboard()->setText( text );
}

void Display::onRecord()
{
    if( !d_recOn )
    {
        qWarning() << "record on";
        d_recOn = true;
        d_record = d_screen.convertToFormat(QImage::Format_RGB32);
    }else
    {
        qWarning() << "record off";
        d_record.save("record.png");
        d_record = QImage();
        d_recOn = false;
    }
}

void Display::onExit()
{
    exit(0);
}

void Display::onLog()
{
    if( s_out.isOpen() )
    {
        qWarning() << "logging off";
        s_out.close();
    }else if( s_out.open(QIODevice::WriteOnly) )
    {
        qWarning() << "logging on";
    }else
        qCritical() << "ERROR: cannot open log for writing";
}

void Display::onBreak()
{
    if( s_break )
        return;
    s_break = true;
    qWarning() << "break started";
}

void Display::onPaste()
{
    const QByteArray text = QApplication::clipboard()->text().toLatin1();
    for( int i = 0; i < text.size(); i++ )
    {
        simulateKeyEvent(text[i]);
    }
}

void Display::onCopy()
{
    s_copy = true;
}

void Display::onPasteBenchmark()
{
    QFile in(":/benchmark/Benchmark.st");
    if( in.open(QIODevice::ReadOnly) )
    {
        const QByteArray text = in.readAll();
        for( int i = 0; i < text.size(); i++ )
        {
            simulateKeyEvent(text[i]);
        }
    }
}

void Display::paintEvent(QPaintEvent* event)
{
    if( d_bitmap.isNull() )
        return;

    const QRect r = event->rect();
    if( r.isNull() )
        return;

    if( !d_updateArea.isNull() )
    {
        d_bitmap.toImage( d_screen, d_updateArea );
        d_updateArea = QRect();
    }

    QPainter p(this);
    p.setRenderHints( QPainter::Antialiasing | QPainter::TextAntialiasing | QPainter::SmoothPixmapTransform, false );

    p.drawImage( r,d_screen, r);
}

void Display::timerEvent(QTimerEvent*)
{
    // update();
}

void Display::closeEvent(QCloseEvent* event)
{
    if( d_forceClose || !s_run )
    {
        event->accept();
        deleteLater();
        return;
    }
    event->ignore();
    const int res = QMessageBox::warning(this, renderTitle(), tr("Do you really want to close the VM? Changes are lost!"),
                                         QMessageBox::Ok | QMessageBox::Cancel, QMessageBox::Cancel );
    if( res == QMessageBox::Ok )
        s_run = false;
}

void Display::mouseMoveEvent(QMouseEvent* event)
{
    QPoint old = d_mousePos;
    d_mousePos = event->pos();
    if( d_mousePos.x() < 0 )
        d_mousePos.setX(0);
    if( d_mousePos.y() < 0 )
        d_mousePos.setY(0);
    if( d_mousePos.x() >= width() )
        d_mousePos.setX( width() - 1 );
    if( d_mousePos.y() >= height() )
        d_mousePos.setY( height() - 1 );

    quint32 diff = PAL2_getTime() - d_lastEvent;
    if( diff < s_msPerFrame )
        return;

    if( old.x() != d_mousePos.x() )
    {
        if( d_mousePos.x() > MaxPos )
            postEvent( XLocation, MaxPos );
        else
            postEvent( XLocation, d_mousePos.x() );
    }
    if( old.y() != d_mousePos.y() )
    {
        if( d_mousePos.y() > MaxPos )
            postEvent( YLocation, MaxPos );
        else
            postEvent( YLocation, d_mousePos.y() );
    }
}

enum MousButton { LeftButton = 130,
                  MidButton = 128, // BB error, mixed up 129 and 128, VIM fixed
                  RightButton = 129
                };

void Display::mousePressEvent(QMouseEvent* event)
{
    mousePressReleaseImp( true, event->button() );
}

void Display::mouseReleaseEvent(QMouseEvent* event)
{
    mousePressReleaseImp( false, event->button() );
}

void Display::mousePressReleaseImp(bool press, int button)
{
    const EventType t = press ? BiStateOn : BiStateOff;

    switch( button )
    {
    case Qt::LeftButton:
        if( QApplication::keyboardModifiers() == 0 )
            postEvent( t, LeftButton );
        else if( QApplication::keyboardModifiers() == Qt::ControlModifier )
            postEvent( t, RightButton );
        else if( QApplication::keyboardModifiers() == ( Qt::ControlModifier | Qt::ShiftModifier ) )
            postEvent( t, MidButton );
        break;
    case Qt::RightButton:
        if( QApplication::keyboardModifiers() == Qt::ShiftModifier )
            postEvent( t, MidButton );
        else
            postEvent( t, RightButton );
        break;
    case Qt::MidButton:
        postEvent( t, MidButton );
        break;
    default:
        break;
    }
}

void Display::keyPressEvent(QKeyEvent* event)
{
    //qDebug() << "keyPressEvent" << QByteArray::number(event->key(),16).constData() << event->text();
    char ch = 0;
    if( !event->text().isEmpty() )
        ch = event->text()[0].toLatin1();
    if( !keyEvent( event->key(), ch, true ) )
        QWidget::keyPressEvent(event);
}

void Display::keyReleaseEvent(QKeyEvent* event)
{
    char ch = 0;
    if( !event->text().isEmpty() )
        ch = event->text()[0].toLatin1();
    if( !keyEvent( event->key(), ch, false ) )
        QWidget::keyReleaseEvent(event);
}

void Display::inputMethodEvent(QInputMethodEvent* event)
{
    QString text = event->commitString();

    if( !text.isEmpty() && text.at(0).isPrint() )
    {
        const char ch = text.at(0).toLatin1();
        keyEvent( 0, ch, true );
        keyEvent( 0, ch, false );
    }
}

QString Display::renderTitle() const
{
    return QString("%1 v%2").arg( QApplication::applicationName() ).arg( QApplication::applicationVersion() );
}

static inline quint16 compose( quint8 t, quint16 p )
{
    return (quint16)t << 12 | p;
}

bool Display::postEvent(Display::EventType t, quint16 param, bool withTime )
{
    Q_ASSERT( t >= XLocation && t <= BiStateOff );

    if( withTime )
    {
        quint32 time = PAL2_getTime();
        quint32 diff = time - d_lastEvent;
        d_lastEvent = time;

        if( diff <= MaxPos )
        {
            d_events.enqueue( compose( DeltaTime, diff ) );
            notify();
        }else
        {
            d_events.enqueue( compose( AbsoluteTime, 0 ) );
            notify();
            d_events.enqueue( ( time >> 16 ) & 0xffff);
            notify();
            d_events.enqueue( time & 0xffff );
            notify();
        }
    }
    d_events.enqueue( compose( t, param ) );
    notify();
    return true;
}

/*
    // Alto keyboard layout
    // see https://www.extremetech.com/wp-content/uploads/2011/10/Alto_Mouse_c.jpg
    1 !
    2 @
    3 #
    4 $
    5 %
    6 ~
    7 &
    8 *
    9 (
    0 )
    - _
    = +
    \ |
    [ {
    ] }
    ← ↑
    ; :
    ' "
    , <
    . >
    / ?
    a A
    ...
    z Z
  */

static inline char toAltoUpper( char ch )
{
    switch( ch )
    {
    case '+':
        return '='; // means: the key is labeled with '=' for normal press and '+' for shift press
                    // if we want a '+' to appear we have to send shift-down '=' shift-up to the VM
    case '_':
        return '-';
    case '|':
        return '\\';
    case '{':
        return '[';
    case '}':
        return ']';
    case ':':
        return ';';
    case '"':
        return '\'';
    case '<':
        return ',';
    case '>':
        return '.';
    case '?':
        return '/';
    case '!':
        return '1';
    case '@':
        return '2';
    case '#':
        return '3';
    case '$':
        return '4';
    case '%':
        return '5';
    case '~':
        return '6';
    case '&':
        return '7';
    case '*':
        return '8';
    case '(':
        return '9';
    case ')':
        return '0';
    }
    if( ch >= 'A' && ch <= 'Z' )
        return ::tolower(ch);
    return 0;
}

static inline bool isAltoLower(char ch )
{
    if( ( ch >= 'a' && ch <= 'z' ) || ( ch >= '0' && ch <= '9' ) )
        return true;
    switch( ch )
    {
    case '-':
    case '=':
    case '\\':
    case '[':
    case ']':
    case ';':
    case '\'':
    case ',':
    case '.':
    case '/':
        return true;
    }
    return false;
}

bool Display::keyEvent(int keyCode, char ch, bool down)
{
    //qDebug() << QByteArray::number(keyCode,16).constData() << ch;
    switch( keyCode )
    {
    case Qt::Key_Backspace:
        return postEvent( down ? BiStateOn : BiStateOff, 8 );
    case Qt::Key_Tab:
        return postEvent( down ? BiStateOn : BiStateOff, 9 );
        // NOTE: line feed	10 not supported
    case Qt::Key_Return:
        return postEvent( down ? BiStateOn : BiStateOff, 13 );
    case Qt::Key_Escape:
        return postEvent( down ? BiStateOn : BiStateOff, 27 );
    case Qt::Key_Space:
        return postEvent( down ? BiStateOn : BiStateOff, 32 );
    case Qt::Key_Delete:
        return postEvent( down ? BiStateOn : BiStateOff, 127 );
        // NOTE: right shift	137
    case Qt::Key_Shift:
        d_shiftDown = down;
        return postEvent( down ? BiStateOn : BiStateOff, 136 );
    case Qt::Key_Control:
        return postEvent( down ? BiStateOn : BiStateOff, 138 );
    case Qt::Key_CapsLock:
        d_capsLockDown = down;
        return postEvent( down ? BiStateOn : BiStateOff, 139 );
    case Qt::Key_Left:
        // ← ASCII 95 0x5f _
        return postEvent( down ? BiStateOn : BiStateOff, 95 );
    case Qt::Key_Up:
        // ↑ ASCII 94 0x5e ^
        return postEvent( down ? BiStateOn : BiStateOff, 94 );
    }
    if( ch >= '!' && ch <= '~' )
    {
        if( isAltoLower( ch ) )
        {
            if( down )
                sendShift( true, false );
            const bool res = postEvent( down ? BiStateOn : BiStateOff, ch );
            if( !down )
                sendShift( false, false );
            return res;
        }else if( ( ch = toAltoUpper( ch ) ) )
        {
            if( down )
                sendShift( true, true );
            const bool res = postEvent( down ? BiStateOn : BiStateOff, ch );
            if( !down )
                sendShift( false, true );
            return res;
        }
    }
    return false;
}

void Display::simulateKeyEvent(char ch)
{
    switch( ch )
    {
    case ' ':
        keyEvent(Qt::Key_Space,0,true);
        keyEvent(Qt::Key_Space,0,false);
        return;
    case '\n':
        keyEvent(Qt::Key_Return,0,true);
        keyEvent(Qt::Key_Return,0,false);
        return;
    case '\r':
        return;
    case 0x08:
        keyEvent(Qt::Key_Backspace,0,true);
        keyEvent(Qt::Key_Backspace,0,false);
        return;
    case 0x09:
        keyEvent(Qt::Key_Tab,0,true);
        keyEvent(Qt::Key_Tab,0,false);
        return;
    case 0x1b:
        keyEvent(Qt::Key_Escape,0,true);
        keyEvent(Qt::Key_Escape,0,false);
        return;
    }
    keyEvent(0,ch,true);
    keyEvent(0,ch,false);
}

void Display::sendShift(bool keyPress, bool shiftRequired)
{
    if( shiftRequired && !d_shiftDown ) // need to press shift
        postEvent( keyPress ? BiStateOn : BiStateOff, 136 );
    else if( !shiftRequired && d_shiftDown ) // need to release shift
        postEvent( !keyPress ? BiStateOn : BiStateOff, 136 );
}

void Display::notify()
{
    emit sigEventQueue();
}

Bitmap::Bitmap(quint8* buf, quint16 wordLen, quint16 pixWidth, quint16 pixHeight)
{
    d_buf = buf;
    d_wordLen = wordLen; // number of words (16 bit) in buffer
    d_pixWidth = pixWidth;
    d_pixHeight = pixHeight;
    d_pixLineWidth = ( ( pixWidth + PixPerWord - 1 ) / PixPerWord ) * PixPerWord;
    // d_pixLineWidth is the d_pixWidth possibly extended by a few pixels so that d_pixLineWidth % 16 = 0
    Q_ASSERT( d_pixLineWidth * d_pixHeight / 16 == d_wordLen );
}

void Bitmap::toImage(QImage& img, QRect area) const
{
    if( isNull() )
        return;
    Q_ASSERT( img.format() == QImage::Format_RGB32 // The image is stored using a 32-bit RGB format (0xffRRGGBB).
              && img.width() == d_pixWidth && img.height() == d_pixHeight );
    // more efficient than Mono because the Qt pipeline has to convert it otherwise

    if( area.isNull() )
        area = img.rect();
    else
    {
        Q_ASSERT( area.x() >= 0 && ( area.x() + area.width() ) <= d_pixWidth );
        Q_ASSERT( area.y() >= 0 && ( area.y() + area.height() ) <= d_pixHeight );
    }

#if 1
    const int sw = d_pixLineWidth / 8;
    const int dw = img.bytesPerLine();
    const uchar *src_data = d_buf;
    uchar *dest_data = img.bits();
    const int ax = area.x();
    const int aw = area.width();
    const int axaw = ax + aw;
    const int ah = area.height();
    const int ay = area.y();

    src_data += sw * ay;
    dest_data += dw * ay;
    for( int y = 0; y < ah; y++ )
    {
        uint*p = (uint*)dest_data;
        p += ax;
        for( int x = ax; x < axaw; x++ )
        {
            int v = ((src_data[x>>3] >> (7 - (x & 7))) & 1);
            if( v )
                *p++ = 0xff000000;
            else
                *p++ = 0xffffffff;
        }
        src_data += sw;
        dest_data += dw;
    }
#else
    const uint8_t* source = d_buf;

    for( int y = 0; y < d_pixHeight; y++ )
    {
        for( int xb = 0; xb < d_pixLineWidth / 8; xb++ )
        {
            std::bitset<8> byte(source[xb]);
            for( int pix = 0; pix < 8; pix++ )
            {
                img.setPixel(xb*8+pix, y, byte.test(pix) ? qRgb(0,0,0) : qRgb(255,255,255));
            }
        }
        source += d_pixLineWidth / 8;
    }

#endif
}

extern "C" {

#ifdef _WIN32
#define DllExport __declspec(dllexport)
#else
#define DllExport
#endif

DllExport void PAL3_setIdle(void (*tick)() )
{
    // NOP
}

DllExport int PAL3_init(uint8_t* b, int len, int w, int h)
{
    Bitmap bm(b, len / 2, w, h);
    Display::inst()->setBitmap(bm);
    return 1;
}

DllExport int PAL3_deinit()
{
    // Display::inst()->getScreen().save("screen_at_end.png"); // TEST
    Display::inst()->forceClose();
    return 0;
}

DllExport int PAL3_setCursorBitmap(uint8_t* b, int w, int h)
{
    Bitmap bm(b, (w * h) / (2 * 8), w, h);
    Display::inst()->setCursorBitmap(bm);
    return 1;
}

DllExport void PAL3_setCursorPos(int x, int y)
{
    Display::inst()->setCursorPos(x,y);
}

DllExport int PAL3_processEvents(int sleep)
{
    Display::processEvents();
    if( !Display::s_run )
        return -1;
    else
        return Display::inst()->eventsPending();
}

DllExport int PAL3_nextEvent()
{
    if( Display::inst()->eventsPending() )
    {
        const quint16 e = Display::inst()->nextEvent();
#if 0
        static int count = 0;
        if( count )
        {
            qWarning() << "cpp AbsoluteTime word" << 3-count;
            count--;
        }else
        {
            QByteArray name;
            const quint16 t = e >> 12;
            switch(t)
            {
            case Display::DeltaTime:
                name = "DeltaTime";
                break;
            case Display::XLocation:
                name = "XLocation";
                break;
            case Display::YLocation:
                name = "YLocation";
                break;
            case Display::BiStateOn:
                name = "BiStateOn";
                break;
            case Display::BiStateOff:
                name = "BiStateOff";
                break;
            case Display::AbsoluteTime:
                count = 2;
                name = "AbsoluteTime";
                break;
            }
            qWarning() << "cpp" << name.constData() << (e & Display::MaxPos) << "pending" << Display::inst()->eventsPending();
        }
#endif
        return e;
    }else
        return 0;
}

DllExport int32_t PAL3_getTime()
{
    return PAL2_getTime();
}

DllExport void PAL3_updateArea(int x,int y,int w,int h,int cx,int cy,int cw,int ch)
{
    QRect r(x,y,w,h);
    QRect clip(cx,cy,cw,ch);
    Display::inst()->updateArea( r & clip );
    QApplication::processEvents();
}

}


