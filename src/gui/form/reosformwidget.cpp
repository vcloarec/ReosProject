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

#include <QApplication>
#include <QClipboard>
#include <QVBoxLayout>
#include <QDialogButtonBox>
#include <QKeyEvent>
#include <QLabel>
#include <QMenu>
#include <QMessageBox>
#include <QMimeData>
#include <QTableView>
#include <QHeaderView>

#include "reosparameterwidget.h"
#include "reosparameter.h"
#include "reostimeserie.h"
#include "reosrainfallintensitydurationwidget.h"
#include "reosidfcurves.h"
#include "reossyntheticrainfall.h"
#include "reosintensitydurationselectedcurvewidget.h"
#include "reosrainfallregistery.h"

ReosFormWidget::ReosFormWidget( QWidget *parent, Qt::Orientation orientation, bool withSpacer ) : QWidget( parent )
{
  setLayout( new QVBoxLayout( this ) );
  switch ( orientation )
  {
    case Qt::Horizontal:
      mMainLayout = new QHBoxLayout( this );
      break;
    case Qt::Vertical:
      mMainLayout = new QVBoxLayout( this );
      break;
  }
  mMainLayout->setContentsMargins( 0, 0, 0, 0 );
  mMainLayout->setSpacing( 6 );
  layout()->setContentsMargins( 0, 0, 0, 0 );
  layout()->addItem( mMainLayout );
  if ( withSpacer )
    layout()->addItem( new QSpacerItem( 10, 10, QSizePolicy::Minimum, QSizePolicy::Expanding ) );
}

void ReosFormWidget::addText( const QString &text, int position )
{
  QLabel *label = new QLabel( text, this );
  label->setWordWrap( true );
  addWidget( label, position );
}

void ReosFormWidget::addParameter( ReosParameter *parameter, int position )
{
  ReosParameterWidget *w = ReosParameterWidget::createWidget( parameter, this );
  if ( !w )
    return;
  addWidget( w, position );
  w->updateValue();
  if ( mParamCount == 0 )
    w->setFocusOnEdit();
  mParamCount++;

  connect( parameter, &ReosParameter::valueChanged, this, &ReosFormWidget::parametersChanged );
}

void ReosFormWidget::addParameters( QList<ReosParameter *> parameters )
{
  for ( ReosParameter *p : qAsConst( parameters ) )
    if ( p )
      addParameter( p );
}

void ReosFormWidget::addData( ReosDataObject *data, int position )
{
  ReosFormWidget *dataWidget = createDataWidget( data, this );
  if ( dataWidget )
    addWidget( dataWidget, position );
}

void ReosFormWidget::addWidget( QWidget *widget, int position )
{
  if ( position >= 0 )
    mMainLayout->insertWidget( position, widget );
  else
    mMainLayout->addWidget( widget );
}

void ReosFormWidget::addItem( QLayoutItem *item, int position )
{
  if ( position >= 0 )
    mMainLayout->insertItem( position, item );
  else
    mMainLayout->addItem( item );
}

ReosFormWidget *ReosFormWidget::createDataWidget( ReosDataObject *dataObject, QWidget *parent )
{
  if ( !dataObject && !ReosFormWidgetRegistery::isInstantiate() )
    return nullptr;

  return ReosFormWidgetRegistery::instance()->createDataFormWidget( dataObject, parent );
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

void ReosFormDialog::addData( ReosDataObject *data )
{
  mForm->addData( data );
}

void ReosFormDialog::addText( const QString &text )
{
  mForm->addText( text );
}

ReosFormWidgetRegistery *ReosFormWidgetRegistery::sInstance = nullptr;

void ReosFormWidgetRegistery::instantiate( ReosModule *parent )
{
  if ( !sInstance )
    sInstance = new ReosFormWidgetRegistery( parent );
}

bool ReosFormWidgetRegistery::isInstantiate()
{
  return sInstance != nullptr;
}

ReosFormWidgetRegistery *ReosFormWidgetRegistery::instance()
{
  return sInstance;
}

void ReosFormWidgetRegistery::addDataWidgetFactory( ReosFormWidgetDataFactory *fact )
{
  for ( const DataWidgetFactory &currentFact : mDataWidgetFactories )
    if ( currentFact->datatype() == fact->datatype() )
      return;

  mDataWidgetFactories.emplace_back( fact );
}

ReosFormWidget *ReosFormWidgetRegistery::createDataFormWidget( ReosDataObject *dataObject, QWidget *parent ) const
{
  if ( !sInstance || !dataObject )
    return nullptr;

  for ( const DataWidgetFactory &fact : mDataWidgetFactories )
    if ( fact->datatype() == dataObject->type() )
      return fact->createDataWidget( dataObject, parent );

  return nullptr;
}

ReosFormWidgetRegistery::ReosFormWidgetRegistery( ReosModule *parent ): ReosModule( parent )
{}
