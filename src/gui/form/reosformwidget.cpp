/***************************************************************************
  reosformwidget.cpp - ReosFormWidget

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
#include "reosformwidget.h"

#include <QVBoxLayout>
#include <QDialogButtonBox>
#include <QTableView>
#include <QHeaderView>

#include "reosparameterwidget.h"
#include "reosparameter.h"
#include "reostimeserie.h"

ReosFormWidget::ReosFormWidget( QWidget *parent ) : QWidget( parent )
{
  setLayout( new QVBoxLayout( this ) );
  layout()->setContentsMargins( 0, 0, 0, 0 );
  layout()->setSpacing( 0 );
}

void ReosFormWidget::addParameter( ReosParameter *parameter )
{
  ReosParameterWidget *w = ReosParameterWidget::createWidget( parameter, this );
  if ( !w )
    return;
  layout()->addWidget( w );
  w->updateValue();
  if ( mParamCount == 0 )
    w->setFocusOnEdit();
  mParamCount++;

  connect( parameter, &ReosParameter::valueChanged, this, &ReosFormWidget::parametersChanged );

}

void ReosFormWidget::addParameters( QList<ReosParameter *> parameters )
{
  for ( ReosParameter *p : qAsConst( parameters ) )
    addParameter( p );
}

void ReosFormWidget::addData( ReosDataObject *data )
{
  ReosFormWidget *dataWidget = createDataWidget( data, this );
  if ( dataWidget )
    layout()->addWidget( dataWidget );
}

ReosFormWidget *ReosFormWidget::createDataWidget( ReosDataObject *dataObject, QWidget *parent )
{
  if ( !dataObject )
    return nullptr;

  if ( dataObject->type() == QStringLiteral( "time-serie-constant-interval" ) )
  {
    ReosTimeSerieConstantInterval *object = qobject_cast<ReosTimeSerieConstantInterval *>( dataObject );
    if ( object )
      return new ReosTimeSerieConstantIntervalWidget( object, parent );
  }

  return nullptr;

}

ReosFormDialog::ReosFormDialog( QWidget *parent ):
  QDialog( parent )
  , mForm( new ReosFormWidget( this ) )
{
  setLayout( new QVBoxLayout );
  layout()->addWidget( mForm );
  QDialogButtonBox *buttons = new QDialogButtonBox( QDialogButtonBox::Ok | QDialogButtonBox::Cancel, this );
  layout()->addWidget( buttons );

  connect( buttons, &QDialogButtonBox::rejected, this, &QDialog::reject );
  connect( buttons, &QDialogButtonBox::accepted, this, &QDialog::accept );
}

void ReosFormDialog::addParameter( ReosParameter *parameter )
{
  mForm->addParameter( parameter );
}


ReosTimeSerieConstantIntervalWidget::ReosTimeSerieConstantIntervalWidget( ReosTimeSerieConstantInterval *timeSerie, QWidget *parent ):
  ReosFormWidget( parent )
  , mModel( new ReosTimeSerieConstantIntervalModel( this ) )
{
  mModel->setSerieData( timeSerie );
  addParameter( timeSerie->timeStep() );
  addParameter( timeSerie->referenceTime() );

  QTableView *view = new QTableView( this );
  layout()->addWidget( view );
  view->setModel( mModel );
  view->horizontalHeader()->setStretchLastSection( true );
  layout()->addWidget( view );
}
