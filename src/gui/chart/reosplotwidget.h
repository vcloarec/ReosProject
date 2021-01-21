/***************************************************************************
  reosplotwidget.h - ReosPlotWidget

 ---------------------
 begin                : 13.1.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSPLOTWIDGET_H
#define REOSPLOTWIDGET_H

#include <QToolBar>
#include <QHBoxLayout>
#include <QApplication>
#include <QClipboard>


class ReosPlot_p;

class QwtPlotMagnifier;
class QwtPlotPanner;
class QwtPlotZoomer;
class QwtPlotPicker;

class QwtPlotItem;
class QwtPlotCurve;



class ReosPlotItem: public QObject
{
    Q_OBJECT
  public:
    virtual void attach( ReosPlot_p *plot );
    virtual ~ReosPlotItem();

  signals:
    void itemChanged();

  protected:
    ReosPlotItem() = default;
    QwtPlotItem *mPlotItem = nullptr;

  private:
    bool mAttached = false;
};

class ReosPlotCurve : public ReosPlotItem
{
  public:
    ReosPlotCurve( const QString &name = QString(), const QColor &color = Qt::black, double width = 1 );
    void setData( const QPolygonF &data );

    void zoomOnExtent();
  private:
    QwtPlotCurve *curve();
};



class ReosPlotWidget: public QWidget
{
    Q_OBJECT
  public:
    enum MagnifierType {NormalMagnifier, PositiveMagnifier};

    ReosPlotWidget( QWidget *parent = nullptr );

    void setLegendVisible( bool b );
    void setMagnifierType( MagnifierType type );
    void setEnableZoomer( bool b );
    void setLegendAlignement( Qt::Alignment align );
    void enableAutoMinimumSize( bool b );
    void setMinimumPlotSize( QSize size );
    void addActions( QList<QAction *> actions );

    void addPlotItem( ReosPlotItem *item );

    void setTitleAxeX( const QString &title );
    void setTitleAxeYleft( const QString &title );
    void setTitleAxeYRight( const QString &title );

  signals:
    void cursorMoved( const QPointF &pt );

  public slots:
    void updatePlot();

  private slots:
    void exportAsImage();
    void copyAsImage();
    void receiveMoveFromPicker( const QPointF &pt );


  private:

    ReosPlot_p *mPlot = nullptr;
    QwtPlotPicker *mPickerTracker;

    QToolBar *mToolBar = nullptr;
    QAction *mActionExportAsImage = nullptr;
    QAction *mActionCopyAsImage = nullptr;
};




#endif // REOSPLOTWIDGET_H
