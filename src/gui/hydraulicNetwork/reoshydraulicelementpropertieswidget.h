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

#include "reosactionwidget.h"

class ReosHydraulicNetworkElement;

namespace Ui
{
  class ReosHydraulicElementPropertiesWidget;
}

class ReosHydraulicElementWidgetFactory : public QObject
{
  public:
    ReosHydraulicElementWidgetFactory( QObject *parent = nullptr ): QObject( parent ) {}
    virtual QWidget *createWidget( ReosHydraulicNetworkElement *element, QWidget *parent = nullptr );
    virtual QString elementType() {return QString();}
};


class ReosHydraulicElementPropertiesWidget : public ReosActionWidget
{
    Q_OBJECT

  public:
    explicit ReosHydraulicElementPropertiesWidget( QWidget *parent = nullptr );
    ~ReosHydraulicElementPropertiesWidget();

  public slots:
    void setCurrentElement( ReosHydraulicNetworkElement *element );

  private:
    Ui::ReosHydraulicElementPropertiesWidget *ui;
    ReosHydraulicNetworkElement *mCurrentElement = nullptr;
    QWidget *mCurrentWidget = nullptr;

    QMap<QString, ReosHydraulicElementWidgetFactory * > mWidgetFactories;
    ReosHydraulicElementWidgetFactory *mDefaultWidgetfactory = nullptr;
    ReosHydraulicElementWidgetFactory *widgetFactory( const QString &elementType );
};




#endif // REOSHYDRAULICELEMENTPROPERTIESWIDGET_H
