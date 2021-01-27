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


ReosTimeSerieConstantIntervalWidget::ReosTimeSerieConstantIntervalWidget( std::weak_ptr<ReosTimeSerieConstantInterval> timeSerie, QWidget *parent ):
  QWidget( parent )
  , mModel( new ReosTimeSerieModel( this ) )
{
  mModel->setSerieData( timeSerie );
  setLayout( new QVBoxLayout );

  QTableView *view = new QTableView( this );
  layout()->addWidget( view );

  view->setModel( mModel );
}
