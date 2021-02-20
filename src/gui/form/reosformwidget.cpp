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
  addWidget( new QLabel( text ), position );
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
  if ( !dataObject )
    return nullptr;

  if ( dataObject->type() == QStringLiteral( "time-serie-constant-interval" ) )
  {
    ReosTimeSerieConstantInterval *object = qobject_cast<ReosTimeSerieConstantInterval *>( dataObject );
    if ( object )
      return new ReosTimeSerieConstantIntervalWidget( object, parent );
  }

  if ( dataObject->type() == QStringLiteral( "chicago-rainfall" ) )
  {
    ReosChicagoRainfall *object = qobject_cast<ReosChicagoRainfall *>( dataObject );
    if ( object )
      return new ReosChicagoRainfallWidget( object, parent );
  }

  if ( dataObject->type() == QStringLiteral( "double-triangle-rainfall" ) )
  {
    ReosDoubleTriangleRainfall *object = qobject_cast<ReosDoubleTriangleRainfall *>( dataObject );
    if ( object )
      return new ReosDoubleTriangleRainfallWidget( object, parent );
  }

  if ( dataObject->type() == QStringLiteral( "rainfall-intensity-duration-curve" ) )
  {
    ReosIntensityDurationCurve *object = qobject_cast<ReosIntensityDurationCurve *>( dataObject );
    if ( object )
      return new ReosRainfallIntensityDurationWidget( object, parent );
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

void ReosFormDialog::addData( ReosDataObject *data )
{
  mForm->addData( data );
}

void ReosFormDialog::addText( const QString &text )
{
  mForm->addText( text );
}


ReosTimeSerieConstantIntervalWidget::ReosTimeSerieConstantIntervalWidget( ReosTimeSerieConstantInterval *timeSerie, QWidget *parent ):
  ReosFormWidget( parent, Qt::Vertical, false )
  , mModel( new ReosTimeSerieConstantIntervalModel( this ) )
{
  mModel->setSerieData( timeSerie );
  addParameter( timeSerie->timeStep() );
  addParameter( timeSerie->referenceTime() );

  mValueModeComboBox = new QComboBox( this );
  mValueModeComboBox->addItem( timeSerie->valueModeName( ReosTimeSerieConstantInterval::Value ), ReosTimeSerieConstantInterval::Value );
  mValueModeComboBox->addItem( timeSerie->valueModeName( ReosTimeSerieConstantInterval::Intensity ), ReosTimeSerieConstantInterval::Intensity );
  mValueModeComboBox->setCurrentIndex( mValueModeComboBox->findData( timeSerie->valueMode() ) );
  mIntensityUnitComboBox = new QComboBox( this );

  mIntensityUnitComboBox->addItem( tr( "millisecond" ), ReosDuration::millisecond );
  mIntensityUnitComboBox->addItem( tr( "second" ), ReosDuration::second );
  mIntensityUnitComboBox->addItem( tr( "minute" ), ReosDuration::minute );
  mIntensityUnitComboBox->addItem( tr( "hour" ), ReosDuration::hour );
  mIntensityUnitComboBox->addItem( tr( "day" ), ReosDuration::day );
  mIntensityUnitComboBox->addItem( tr( "week" ), ReosDuration::week );
  mIntensityUnitComboBox->addItem( tr( "month" ), ReosDuration::month );
  mIntensityUnitComboBox->addItem( tr( "year" ), ReosDuration::year );
  mIntensityUnitComboBox->setCurrentIndex( mIntensityUnitComboBox->findData( timeSerie->intensityTimeUnit() ) );
  mIntensityUnitComboBox->setEnabled( timeSerie->valueMode() == ReosTimeSerieConstantInterval::Intensity );

  QHBoxLayout *layoutIntUnit = new QHBoxLayout( this );
  layoutIntUnit->addWidget( new QLabel( tr( "Intenstity time unit" ), this ) );
  layoutIntUnit->addWidget( mIntensityUnitComboBox );
  addItem( layoutIntUnit );
  QHBoxLayout *layoutMode = new QHBoxLayout( this );
  layoutMode->addWidget( new QLabel( tr( "Value type" ), this ) );
  layoutMode->addWidget( mValueModeComboBox );
  addItem( layoutMode );

  connect( mValueModeComboBox, QOverload<int>::of( &QComboBox::currentIndexChanged ), timeSerie, [timeSerie, this]()
  {
    ReosTimeSerieConstantInterval::ValueMode mode = static_cast<ReosTimeSerieConstantInterval::ValueMode>( this->mValueModeComboBox->currentData().toInt() );
    timeSerie->setValueMode( mode );
    mIntensityUnitComboBox->setEnabled( mode == ReosTimeSerieConstantInterval::Intensity );
  } );

  connect( mIntensityUnitComboBox, QOverload<int>::of( &QComboBox::currentIndexChanged ), timeSerie, [timeSerie, this]()
  {
    ReosDuration::Unit unit = static_cast<ReosDuration::Unit>( this->mIntensityUnitComboBox->currentData().toInt() );
    timeSerie->setIntensityTimeUnit( unit );
  } );

  ReosTimeSerieConstantIntervalView *view = new ReosTimeSerieConstantIntervalView( this );
  addWidget( view );
  view->setModel( mModel );
  view->horizontalHeader()->setStretchLastSection( true );
  view->horizontalHeader()->setCascadingSectionResizes( true );

  connect( view, &ReosTimeSerieConstantIntervalView::pastDataFromClipboard, mModel, &ReosTimeSerieConstantIntervalModel::setValues );
  connect( view, &ReosTimeSerieConstantIntervalView::insertRow, mModel, &ReosTimeSerieConstantIntervalModel::insertValueRows );
  connect( view, &ReosTimeSerieConstantIntervalView::deleteRows, mModel, &ReosTimeSerieConstantIntervalModel::deleteValueRows );

}

ReosTimeSerieConstantIntervalView::ReosTimeSerieConstantIntervalView( QWidget *parent ): QTableView( parent )
{
}

void ReosTimeSerieConstantIntervalView::keyPressEvent( QKeyEvent *event )
{
  if ( event->matches( QKeySequence::Paste ) )
  {
    QList<double> values = clipboardToValues();
    if ( !values.isEmpty() )
      emit pastDataFromClipboard( currentIndex(), values );
  }


  QTableView::keyPressEvent( event );

  if ( event->key() == Qt::Key_Enter )
  {
    QModelIndex index = currentIndex();
    if ( index.isValid() )
      setCurrentIndex( model()->index( index.row() + 1, index.column() ) );
  }
}

void ReosTimeSerieConstantIntervalView::contextMenuEvent( QContextMenuEvent *event )
{
  QMenu menu;

  menu.addAction( tr( "Delete selected row(s)" ), this, [this]()
  {
    int count = this->selectionModel()->selectedIndexes().count();
    if ( count > 0 )
    {
      emit deleteRows( this->selectionModel()->selectedIndexes().first(), count );
    }
  } );

  menu.addAction( tr( "Insert row(s)" ), this, [this]()
  {
    int count = this->selectionModel()->selectedIndexes().count();
    if ( count > 0 )
    {
      emit insertRow( this->selectionModel()->selectedIndexes().first(), count );
    }
  } );

  menu.addAction( tr( "Insert row(s) from clipboard" ), this, [this]()
  {
    int count = this->selectionModel()->selectedIndexes().count();
    QList<double> values = clipboardToValues();
    if ( values.count() > 0 && count > 0 )
    {
      emit insertRow( this->selectionModel()->selectedIndexes().first(), count );
      emit pastDataFromClipboard( this->selectionModel()->selectedIndexes().first(), values );
    }
  } );

  menu.exec( mapToGlobal( event->pos() ) );

}

QList<double> ReosTimeSerieConstantIntervalView::clipboardToValues()
{
  QClipboard *clipBoard = QApplication::clipboard();
  QString clipBoardText = clipBoard->text();
  if ( QLocale::system().decimalPoint() == ',' )
    clipBoardText.replace( ',', '.' );
  const QStringList lines = clipBoardText.split( "\n" );
  QList<double> values;
  bool ok = true;
  for ( const QString &l : lines )
  {
    if ( l == QString() )
      continue;
    values.append( l.toDouble( &ok ) );
    if ( !ok )
      break;
  }
  if ( !ok )
  {
    QMessageBox::warning( this, tr( "Paste value to table" ), tr( "Incompatible data in the cliboard: " ).arg( clipBoardText ) );
    return QList<double>();
  }
  else
    return values;
}

ReosChicagoRainfallWidget::ReosChicagoRainfallWidget( ReosChicagoRainfall *rainfall, QWidget *parent ):
  ReosTimeSerieConstantIntervalWidget( rainfall, parent ),
  mIdfWidget( new ReosIntensityDurationSelectedCurveWidget( this ) )
{
  addParameter( rainfall->totalDuration(), 1 );

  if ( ReosRainfallRegistery::isInstantiate() )
  {
    ReosRainfallIntensityDurationCurveItem *curveItem =
      qobject_cast<ReosRainfallIntensityDurationCurveItem *>( ReosRainfallRegistery::instance()->item( rainfall->intensityDurationUri() ) );
    if ( curveItem )
      mIdfWidget->setCurveItem( curveItem );
    else
      mIdfWidget->clearCurveItem();
  }

  connect( mIdfWidget, &ReosIntensityDurationSelectedCurveWidget::curveChanged, rainfall, [rainfall, this]
  {
    if ( mIdfWidget->curveItem() )
      rainfall->setIntensityDurationCurve( this->mIdfWidget->curveItem()->data(), this->mIdfWidget->curveItem()->uri() );
  } );

  addWidget( mIdfWidget, 3 );
  addParameter( rainfall->centerCoefficient() );
}

ReosDoubleTriangleRainfallWidget::ReosDoubleTriangleRainfallWidget( ReosDoubleTriangleRainfall *rainfall, QWidget *parent ):
  ReosTimeSerieConstantIntervalWidget( rainfall, parent ),
  mIntenseIdfWidget( new ReosIntensityDurationSelectedCurveWidget( this ) ),
  mTotalIdfWidget( new ReosIntensityDurationSelectedCurveWidget( this ) )
{
  addParameter( rainfall->intenseDuration(), 1 );
  addParameter( rainfall->totalDuration(), 2 );

  mIntenseIdfWidget->setTitle( tr( "Intense Intensity Duration Curve" ) );
  mTotalIdfWidget->setTitle( tr( "Total Intensity Duration Curve" ) );

  if ( ReosRainfallRegistery::isInstantiate() )
  {
    ReosRainfallIntensityDurationCurveItem *intenseCurveItem =
      qobject_cast<ReosRainfallIntensityDurationCurveItem *>( ReosRainfallRegistery::instance()->item( rainfall->intensityDurationUriIntense() ) );
    if ( intenseCurveItem )
      mIntenseIdfWidget->setCurveItem( intenseCurveItem );
    else
      mIntenseIdfWidget->clearCurveItem();

    ReosRainfallIntensityDurationCurveItem *totalCurveItem =
      qobject_cast<ReosRainfallIntensityDurationCurveItem *>( ReosRainfallRegistery::instance()->item( rainfall->intensityDurationUriTotal() ) );
    if ( totalCurveItem )
      mTotalIdfWidget->setCurveItem( totalCurveItem );
    else
      mTotalIdfWidget->clearCurveItem();
  }

  connect( mIntenseIdfWidget, &ReosIntensityDurationSelectedCurveWidget::curveChanged, rainfall, [rainfall, this]
  {
    if ( this->mIntenseIdfWidget->curveItem() && this->mTotalIdfWidget->curveItem() )
      rainfall->setIntensityDurationCurve( this->mIntenseIdfWidget->curveItem()->data(),
                                           this->mTotalIdfWidget->curveItem()->data(),
                                           this->mIntenseIdfWidget->curveItem()->uri(),
                                           this->mTotalIdfWidget->curveItem()->uri() );
  } );

  connect( mTotalIdfWidget, &ReosIntensityDurationSelectedCurveWidget::curveChanged, rainfall, [rainfall, this]
  {
    if ( this->mIntenseIdfWidget->curveItem() && this->mTotalIdfWidget->curveItem() )
      rainfall->setIntensityDurationCurve( this->mIntenseIdfWidget->curveItem()->data(),
                                           this->mTotalIdfWidget->curveItem()->data(),
                                           this->mIntenseIdfWidget->curveItem()->uri(),
                                           this->mTotalIdfWidget->curveItem()->uri() );
  } );

  addWidget( mIntenseIdfWidget, 4 );
  addWidget( mTotalIdfWidget, 4 );
  addParameter( rainfall->centerCoefficient() );
}
