/***************************************************************************
  reosformwidget.h - ReosFormWidget

 ---------------------
 begin                : 25.1.2021
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
#ifndef REOSFORMWIDGET_H
#define REOSFORMWIDGET_H

#include <QWidget>
#include <QTableView>
#include <QDialog>
#include <QComboBox>

#include "reosmodule.h"

class QLayoutItem;
class QBoxLayout;
class ReosParameter;
class ReosDataObject;
class ReosTimeSerieConstantInterval;
class ReosTimeSerieConstantIntervalModel;
class ReosChicagoRainfall;
class ReosDoubleTriangleRainfall;
class ReosIntensityDurationSelectedCurveWidget;


class ReosFormWidget : public QWidget
{
    Q_OBJECT
  public:
    explicit ReosFormWidget( QWidget *parent = nullptr, Qt::Orientation orientation = Qt::Vertical, bool withSpacer = true );

    void addText( const QString &text, int position = -1 );
    void addParameter( ReosParameter *parameter, int position = -1 );
    void addParameters( QList<ReosParameter *> parameters );
    void addData( ReosDataObject *data, int position = -1 );
    void addWidget( QWidget *widget, int position = -1 );
    void addItem( QLayoutItem *item, int position = -1 );

    static ReosFormWidget *createDataWidget( ReosDataObject *dataObject, QWidget *parent = nullptr );

  signals:
    void parametersChanged();

  private:
    int mParamCount = 0;
    QBoxLayout *mMainLayout = nullptr;

};


class ReosFormDialog : public QDialog
{
    Q_OBJECT
  public:
    explicit ReosFormDialog( QWidget *parent = nullptr );
    void addParameter( ReosParameter *parameter );
    void addData( ReosDataObject *data );
    void addText( const QString &text );

  private:
    ReosFormWidget *mForm = nullptr;
};


class ReosFormWidgetDataFactory
{
  public:
    virtual ReosFormWidget *createDataWidget( ReosDataObject *dataObject, QWidget *parent ) = 0;
    virtual QString datatype() const = 0;
};

class ReosFormWidgetRegistery: public ReosModule
{
  public:
    static void instantiate( ReosModule *parent );
    static bool isInstantiate();
    static ReosFormWidgetRegistery *instance();

    void addDataWidgetFactory( ReosFormWidgetDataFactory *fact );
    ReosFormWidget *createDataFormWidget( ReosDataObject *dataObject, QWidget *parent ) const;

  private:
    ReosFormWidgetRegistery( ReosModule *parent );
    static ReosFormWidgetRegistery *sInstance;
    using DataWidgetFactory = std::unique_ptr<ReosFormWidgetDataFactory>;
    std::vector<DataWidgetFactory> mDataWidgetFactories;
};





#endif // REOSFORMWIDGET_H
