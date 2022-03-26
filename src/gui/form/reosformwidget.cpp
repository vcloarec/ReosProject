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
  setLayout( new QVBoxLayout );
  switch ( orientation )
  {
    case Qt::Horizontal:
      mMainLayout = new QHBoxLayout( );
      break;
    case Qt::Vertical:
      mMainLayout = new QVBoxLayout( );
      break;
  }
  mMainLayout->setContentsMargins( 0, 0, 0, 0 );
  mMainLayout->setSpacing( 6 );
  layout()->setContentsMargins( 0, 0, 0, 0 );
  layout()->addItem( mMainLayout );
  if ( withSpacer )
    layout()->addItem( new QSpacerItem( 0, 0, QSizePolicy::MinimumExpanding, QSizePolicy::MinimumExpanding ) );
}

void ReosFormWidget::addText( const QString &text, int position )
{
  QLabel *label = new QLabel( text, this );
  label->setWordWrap( true );
  addWidget( label, position );
}

ReosParameterWidget *ReosFormWidget::addParameter( ReosParameter *parameter, int position, ReosParameterWidget::SpacerPosition spacer )
{
  ReosParameterWidget *w = ReosParameterWidget::createWidget( parameter, this );
  if ( !w )
    return nullptr;
  addWidget( w, position );
  w->updateValue();
  if ( mParamCount == 0 )
    w->setFocusOnEdit();
  mParamCount++;

  connect( parameter, &ReosParameter::valueChanged, this, &ReosFormWidget::parametersChanged );
  w->enableSpacer( spacer );

  return w;
}

void ReosFormWidget::addParameters( QList<ReosParameter *> parameters, ReosParameterWidget::SpacerPosition spacer )
{
  for ( ReosParameter *p : qAsConst( parameters ) )
    if ( p )
      addParameter( p, spacer );
}

ReosFormWidget *ReosFormWidget::addData( ReosDataObject *data, const ReosGuiContext &context, int position )
{
  ReosFormWidget *dataWidget = createDataWidget( data, ReosGuiContext( context, this ) );
  if ( dataWidget )
    addWidget( dataWidget, position );

  return dataWidget;
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

void ReosFormWidget::addLine( int position )
{
  QFrame *line = new QFrame( this );
  line->setFrameShape( mOrientation == Qt::Vertical ? QFrame::HLine : QFrame::VLine );
  line->setFrameShadow( QFrame::Sunken );
  addWidget( line, position );
}

ReosFormWidget *ReosFormWidget::createDataWidget( ReosDataObject *dataObject, const ReosGuiContext &guiContext )
{
  if ( !dataObject && !ReosFormWidgetFactories::isInstantiate() )
    return nullptr;

  return ReosFormWidgetFactories::instance()->createDataFormWidget( dataObject, guiContext );
}

void ReosFormWidget::setStretch( int i, int stretch )
{
  mMainLayout->setStretch( i, stretch );
}

int ReosFormWidget::count() const
{
  return mMainLayout->count();
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

void ReosFormDialog::addWidget( QWidget *widget )
{
  mForm->addWidget( widget );
}

ReosFormWidget *ReosFormDialog::addData( ReosDataObject *data, const ReosGuiContext &context )
{
  return mForm->addData( data, context );
}

void ReosFormDialog::addText( const QString &text )
{
  mForm->addText( text );
}

ReosFormWidgetFactories *ReosFormWidgetFactories::sInstance = nullptr;

void ReosFormWidgetFactories::instantiate( ReosModule *parent = nullptr )
{
  if ( !sInstance )
    sInstance = new ReosFormWidgetFactories( parent );
}

bool ReosFormWidgetFactories::isInstantiate()
{
  return sInstance != nullptr;
}

ReosFormWidgetFactories *ReosFormWidgetFactories::instance()
{
  if ( !sInstance )
    instantiate();
  return sInstance;
}

void ReosFormWidgetFactories::addDataWidgetFactory( ReosFormWidgetDataFactory *fact )
{
  for ( const DataWidgetFactory &currentFact : mDataWidgetFactories )
    if ( currentFact->datatype() == fact->datatype() )
      return;
    else if ( fact->datatype().contains( currentFact->datatype() ) )
    {
      currentFact->addSubFactory( fact );
      return;
    }

  mDataWidgetFactories.emplace_back( fact );
}

ReosFormWidget *ReosFormWidgetFactories::createDataFormWidget( ReosDataObject *dataObject, const ReosGuiContext &guiContext ) const
{
  if ( !sInstance || !dataObject )
    return nullptr;

  for ( const DataWidgetFactory &fact : mDataWidgetFactories )
    if ( dataObject->type().contains( fact->datatype() ) )
      return fact->createWidget( dataObject, guiContext );

  return nullptr;
}

ReosFormWidgetFactories::ReosFormWidgetFactories( ReosModule *parent ): ReosModule( parent )
{}

ReosFormWidget *ReosFormWidgetDataFactory::createDataWidget( ReosDataObject *, const ReosGuiContext & )
{
  return nullptr;
}

ReosFormWidget *ReosFormWidgetDataFactory::createWidget( ReosDataObject *dataObject, const ReosGuiContext &guiContext )
{
  for ( const DataWidgetFactory &currentFact : mSubDataWidgetFactories )
    if ( dataObject->type().contains( currentFact->datatype() ) )
      return currentFact->createWidget( dataObject, guiContext );

  return createDataWidget( dataObject, guiContext );
}

void ReosFormWidgetDataFactory::addSubFactory( ReosFormWidgetDataFactory *fact )
{
  for ( const DataWidgetFactory &currentFact : mSubDataWidgetFactories )
    if ( currentFact->datatype() == fact->datatype() )
      return;
    else if ( fact->datatype().contains( currentFact->datatype() ) )
    {
      currentFact->addSubFactory( fact );
      return;
    }

  mSubDataWidgetFactories.emplace_back( fact );
}
