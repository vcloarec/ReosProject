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
#include <QDialog>

class ReosParameter;
class ReosDataObject;
class ReosTimeSerieConstantInterval;
class ReosTimeSerieConstantIntervalModel;


class ReosFormWidget : public QWidget
{
    Q_OBJECT
  public:
    explicit ReosFormWidget( QWidget *parent = nullptr );

    void addParameter( ReosParameter *parameter );
    void addParameters( QList<ReosParameter *> parameters );
    void addData( ReosDataObject *data );


    static ReosFormWidget *createDataWidget( ReosDataObject *dataObject, QWidget *parent = nullptr );

  signals:
    void parametersChanged();

  private:
    int mParamCount = 0;

};


class ReosFormDialog : public QDialog
{
    Q_OBJECT
  public:
    explicit ReosFormDialog( QWidget *parent = nullptr );
    void addParameter( ReosParameter *parameter );

  private:
    ReosFormWidget *mForm = nullptr;

};

class ReosTimeSerieConstantIntervalWidget: public ReosFormWidget
{
  public:
    explicit ReosTimeSerieConstantIntervalWidget( ReosTimeSerieConstantInterval *timeSerie, QWidget *parent );

  private:
    ReosTimeSerieConstantIntervalModel *mModel = nullptr;

};

#endif // REOSFORMWIDGET_H
