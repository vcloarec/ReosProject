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

#include <memory>

#include <QPointer>
#include <QToolBar>
#include <QToolButton>
#include <QHBoxLayout>
#include <QApplication>
#include <QClipboard>

#include "reosmodule.h"
#include "reosgui.h"
#include "reossettings.h"

class QComboBox;
class QSpinBox;

class QwtPlotMagnifier;
class QwtPlotPanner;
class QwtPlotZoomer;
class QwtPlotPicker;
class QwtPlotItem;
class QwtPlotCurve;
class ReosPlotCurve_p;

class ReosPlot_p;
class ReosDataObject;
class ReosTimeSerieConstantInterval;
class ReosVariableTimeStepPlotListButton;
class ReosPlotTimeLine;
class CoordinatesWidget;

class ReosPlotWidget;

class ReosPlotLegendController : public QToolButton
{
    Q_OBJECT
  public:
    ReosPlotLegendController( ReosPlotWidget *plotWidget );
    void setLegendEnabled( bool b );

    void setCurrentAlignment( Qt::Alignment alignment );
    void setCurrentColumnCount( int columnCount );

  signals:
    void legendVisible( bool b );
  private:
    ReosPlotWidget *mPlotWidget = nullptr;
    QSpinBox *mColumnSpinBox = nullptr;
    QList<Qt::Alignment> mLegendAlignments;
    QList<QToolButton *> mAlignmentButtons;
};

class REOSGUI_EXPORT ReosPlotItem: public QObject
{
    Q_OBJECT
  public:
    virtual void attach( ReosPlot_p *plot );
    virtual void detach();
    virtual ~ReosPlotItem();

    void setOnRightAxe();
    bool isOnRightAxe();

    void setOnLeftAxe();
    bool isOnLeftAxe();

    /**
     *  For Item that can control settings of the plot, set whether this item is a master,
     *  that is can control settings of the plot (axes, title,...)
     */
    void setAsMasterItem( bool b );

    //! Set if the item has to be taken into account when autoscale axes
    void setAutoScale( bool b );

    virtual QString name() const;

    void setVisible( bool isVisible, bool replot = true );
    bool isVisible() const;

    void setLegendActive( bool legendActive, bool updateLegend = true );

    void setZ( double z );

    virtual QColor color() const {return QColor();}
    virtual QPixmap icone( const QSize &size ) const;

  public slots:
    virtual void fullExtent();
    virtual void setSettings() {};
    void setName( const QString &name );
    virtual void setColor( const QColor &color );
    virtual void setStyle( Qt::PenStyle style );
    virtual void setWidth( double width );

  signals:
    void itemChanged();

  protected:
    ReosPlotItem() = default;
    QwtPlotItem *mPlotItem = nullptr;
    bool mMasterItem = false;

  private:
    bool mAttached = false;
};

class ReosPlotCurve : public ReosPlotItem
{
  public:
    ReosPlotCurve( const QString &name = QString(), const QColor &color = Qt::black, double width = 1 );
    void setData( const QPolygonF &data );

    void zoomOnExtent();
    void setWidth( double width ) override;
  private:
    ReosPlotCurve_p *curve();
};


class REOSGUI_EXPORT ReosPlotWidget: public QWidget
{
    Q_OBJECT
  public:
    enum MagnifierType {normalMagnifier, positiveMagnifier};

    enum AxeType
    {
      normal,
      temporal,
      logarithm
    };

    ReosPlotWidget( QWidget *parent = nullptr );
    ~ReosPlotWidget();

    void setSettingsContext( const QString &settingContext );

    void setLegendEnabled( bool b );
    void setLegendAlignement( Qt::Alignment align );
    void setLegendColumnCount( int columnCount );
    void setMagnifierType( MagnifierType type );
    void enableAutoMinimumSize( bool b );
    void setMinimumPlotSize( const QSize &size );
    void addActions( const QList<QAction *> &actions );

    void addPlotItem( ReosPlotItem *item );
    void addOptionalPlotItem( ReosVariableTimeStepPlotListButton *optionalItemButton );

    void setTitleAxeX( const QString &title );
    void setTitleAxeYLeft( const QString &title );
    void setTitleAxeYRight( const QString &title );

    void enableAxeYRight( bool b );

    void enableAutoScale( bool b );
    void enableAutoScaleX( bool b );
    void enableAutoScaleY( bool b );

    void setAxeXType( AxeType type );
    void setAxeXExtent( double min, double max );
    void setAxeXExtent( const QDateTime &timeMin, const QDateTime &timeMax );
    void setAxeYLeftExtent( double min, double max );
    void setAxeYRightExtent( double min, double max );

    void setAxesTextSize( int size );
    void setAxesTitleSize( int size );

    void enableScaleTypeChoice( bool b );

    void enableTimeLine( bool b );
    void setTime( const QDateTime &time );

    void resetZoomBase();

    static QString plotEngineName();
    static QString plotEngineVersion();
    static QString plotEngineLink();

  signals:
    void cursorMoved( const QPointF &pt );

  public slots:
    void updatePlot();

  private slots:
    void exportAsImage();
    void copyAsImage();
    void receiveMoveFromPicker( const QPointF &pt );
    void setLegendVisible( bool b );
    void setTimeLineVisible( bool b );
    void enableCursorCoordinates( bool b );

  private:
    QString mSettingsContext;
    ReosPlot_p *mPlot = nullptr;
    QwtPlotPicker *mPickerTracker;

    QToolBar *mToolBarRight = nullptr;
    QToolBar *mToolBarLeft = nullptr;
    QAction *mActionExportAsImage = nullptr;
    QAction *mActionCopyAsImage = nullptr;
    QAction *mXAxisFormatCombobox = nullptr;
    QAction *mActionTimeLine = nullptr;
    QAction *mActionCoordinates = nullptr;

    AxeType mAxeType = normal;

    QwtPlotMagnifier *mMagnifier = nullptr;
    QwtPlotPanner *mPanner = nullptr;
    QwtPlotZoomer *mZoomerLeft = nullptr;
    QwtPlotZoomer *mZoomerRight = nullptr;

    CoordinatesWidget *mCoordinatesWidget = nullptr;

    std::unique_ptr<ReosPlotTimeLine> mTimeLine;

    ReosPlotLegendController *mLegendController = nullptr;
    QAction *mActionLegendController = nullptr;

    QString settingsPrefix() const;
};

class ReosDataPlotItemFactory
{
  public:
    virtual QString datatype() const = 0;
    virtual void buildPlotItemsAndSetup( ReosPlotWidget *plotWidget, ReosDataObject *data );
    virtual ReosPlotItem *buildPlotItem( ReosPlotWidget *plotWidget, ReosDataObject *data );
};


class REOSGUI_EXPORT ReosPlotItemFactories: public ReosModule
{
  public:
    static void instantiate( ReosModule *parent = nullptr );
    static bool isInstantiate();
    static ReosPlotItemFactories *instance();

    void addFactory( ReosDataPlotItemFactory *fact );
    void buildPlotItemsAndSetup( ReosPlotWidget *plotWidget, ReosDataObject *data, const QString &dataType = QString() );
    ReosPlotItem *buildPlotItem( ReosPlotWidget *plotWidget, ReosDataObject *data );

  private:
    ReosPlotItemFactories( ReosModule *parent = nullptr );
    static ReosPlotItemFactories *sInstance;
    using Factory = std::unique_ptr<ReosDataPlotItemFactory>;
    std::vector<Factory> mFactories;

};



#endif // REOSPLOTWIDGET_H
