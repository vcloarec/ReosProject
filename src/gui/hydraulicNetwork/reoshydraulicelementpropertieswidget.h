/***************************************************************************
  reoshydraulicelementpropertieswidget.h - ReosHydraulicElementPropertiesWidget

 ---------------------
 begin                : 25.5.2021
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
#ifndef REOSHYDRAULICELEMENTPROPERTIESWIDGET_H
#define REOSHYDRAULICELEMENTPROPERTIESWIDGET_H

#include <QWidget>
#include <QMap>
#include <QPointer>

#include "reosactionwidget.h"
#include "reosguicontext.h"

class QComboBox;
class QProgressBar;
class QTimer;

class ReosHydraulicNetworkElement;
class ReosWatershedModule;
class ReosCalculationContext;
class ReosHydraulicElementWidget;
class ReosHydraulicNetwork;

namespace Ui
{
  class ReosHydraulicElementPropertiesWidget;
}


class ReosHydraulicElementWidgetFactory : public QObject
{
  public:
    ReosHydraulicElementWidgetFactory( QObject *parent = nullptr ): QObject( parent ) {}
    virtual ReosHydraulicElementWidget *createWidget( ReosHydraulicNetworkElement *element, const ReosGuiContext &context = ReosGuiContext() );
    virtual QString elementType() {return QString();}
};

class ReosHydrauylicNetworkElementCalculationControler: public QObject
{
    Q_OBJECT
  public:
    ReosHydrauylicNetworkElementCalculationControler( ReosHydraulicNetworkElement *element, QObject *parent = nullptr );

    void setProgressBar( QProgressBar *progBar );

  private slots:
    void onCalculationStart();
    void updateState();
    void onCalculationStop();

  private:
    QPointer<ReosHydraulicNetworkElement> mElement;
    QPointer<QProgressBar> mProgessBar;
    QTimer *mTimer = nullptr;
};


class ReosHydraulicElementPropertiesWidget : public ReosStackedPageWidget
{
    Q_OBJECT

  public:
    explicit ReosHydraulicElementPropertiesWidget( ReosHydraulicNetwork *network, const ReosGuiContext &guiContext );
    ~ReosHydraulicElementPropertiesWidget();

    ReosHydraulicElementWidget *currentWidget() const;

  public slots:
    void setCurrentElement( ReosHydraulicNetworkElement *element, const ReosGuiContext &guiContext );

  private slots:
    void updateElementCalculation();
    void setTime( const QDateTime &time );

  private:
    ReosMap *mMap = nullptr;
    ReosHydraulicNetworkElement *mCurrentElement = nullptr;
    ReosHydraulicElementWidget *mCurrentWidget = nullptr;
    QWidget *mNameParameterWidget = nullptr;
    QLayout *mMainLayout = nullptr;
    QLayout *mNameLayout = nullptr;
    ReosHydraulicNetwork *mNetwork = nullptr;

    QMap<QString, ReosHydraulicElementWidgetFactory * > mWidgetFactories;
    ReosHydraulicElementWidgetFactory *mDefaultWidgetfactory = nullptr;

    ReosHydraulicElementWidgetFactory *widgetFactory( const QString &elementType );
    void addWidgetFactory( ReosHydraulicElementWidgetFactory *factory );
};

class ReosHydraulicElementPropertiesActionWidget : public ReosActionStackedWidget
{
    Q_OBJECT
  public:
    ReosHydraulicElementPropertiesActionWidget( ReosHydraulicNetwork *network, const ReosGuiContext &guiContext );

    ReosHydraulicElementWidget *currentElementWidget() const;

  public slots:
    void setCurrentElement( ReosHydraulicNetworkElement *element, const ReosGuiContext &guiContext );

  private:
    ReosHydraulicElementPropertiesWidget *mainPage = nullptr;
    ReosHydraulicNetworkElement *mCurrentElement = nullptr;
};


#endif // REOSHYDRAULICELEMENTPROPERTIESWIDGET_H
