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

class QComboBox;
class QProgressBar;
class QTimer;

class ReosHydraulicNetworkElement;
class ReosWatershedModule;
class ReosCalculationContext;
class ReosHydraulicElementWidget;

namespace Ui
{
  class ReosHydraulicElementPropertiesWidget;
}


class ReosHydraulicElementWidgetFactory : public QObject
{
  public:
    ReosHydraulicElementWidgetFactory( QObject *parent = nullptr ): QObject( parent ) {}
    virtual ReosHydraulicElementWidget *createWidget( ReosHydraulicNetworkElement *element, QWidget *parent = nullptr );
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


class ReosHydraulicElementPropertiesWidget : public ReosActionWidget
{
    Q_OBJECT

  public:
    explicit ReosHydraulicElementPropertiesWidget( ReosWatershedModule *watershedModule, QWidget *parent = nullptr );
    ~ReosHydraulicElementPropertiesWidget();

  public slots:
    void setCurrentElement( ReosHydraulicNetworkElement *element );

  private slots:
    void updateElementCalculation();

  private:
    Ui::ReosHydraulicElementPropertiesWidget *ui;
    ReosHydraulicNetworkElement *mCurrentElement = nullptr;
    QComboBox *mMeteoModelCombo = nullptr;
    ReosHydraulicElementWidget *mCurrentWidget = nullptr;
    QWidget *mNameParameterWidget = nullptr;
    QLayout *mMainLayout = nullptr;
    QLayout *mNameLayout = nullptr;

    QMap<QString, ReosHydraulicElementWidgetFactory * > mWidgetFactories;
    ReosHydraulicElementWidgetFactory *mDefaultWidgetfactory = nullptr;

    ReosHydraulicElementWidgetFactory *widgetFactory( const QString &elementType );
    void addWidgetFactory( ReosHydraulicElementWidgetFactory *factory );

};




#endif // REOSHYDRAULICELEMENTPROPERTIESWIDGET_H
