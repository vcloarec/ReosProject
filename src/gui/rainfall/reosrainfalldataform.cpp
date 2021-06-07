/***************************************************************************
  reosrainfalldataform.cpp - %{Cpp:License:ClassName}

 ---------------------
 begin                : 25.2.2021
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

#include <QApplication>
#include <QKeyEvent>
#include <QHeaderView>
#include <QHBoxLayout>
#include <QLabel>
#include <QMenu>
#include <QMessageBox>
#include <QClipboard>

#include "reosrainfalldataform.h"
#include "reostimeserie.h"
#include "reosidfcurves.h"
#include "reossyntheticrainfall.h"
#include "reosintensitydurationselectedcurvewidget.h"
#include "reosrainfallintensitydurationwidget.h"
#include "reosrainfallregistery.h"


ReosTimeSerieConstantIntervalView::ReosTimeSerieConstantIntervalView( QWidget *parent ): QTableView( parent )
{
  setHorizontalHeader( new ReosHorizontalHeaderView( this ) );
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

  menu.exec( viewport()->mapToGlobal( event->pos() ) );

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
  layoutIntUnit->addWidget( new QLabel( tr( "Intensity time unit" ), this ) );
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
  view->horizontalHeader()->setDefaultAlignment( Qt::AlignCenter | ( Qt::Alignment )Qt::TextWordWrap );

  connect( view, &ReosTimeSerieConstantIntervalView::pastDataFromClipboard, mModel, &ReosTimeSerieConstantIntervalModel::setValues );
  connect( view, &ReosTimeSerieConstantIntervalView::insertRow, mModel, &ReosTimeSerieConstantIntervalModel::insertValueRows );
  connect( view, &ReosTimeSerieConstantIntervalView::deleteRows, mModel, &ReosTimeSerieConstantIntervalModel::deleteValueRows );
}


ReosChicagoRainfallWidget::ReosChicagoRainfallWidget( ReosChicagoRainfall *rainfall, QWidget *parent ):
  ReosTimeSerieConstantIntervalWidget( rainfall, parent ),
  mIdfWidget( new ReosIntensityDurationSelectedCurveWidget( this ) )
{
  addParameter( rainfall->totalDuration(), 1 );

  if ( ReosRainfallRegistery::isInstantiate() )
  {
    ReosRainfallIntensityDurationCurveItem *curveItem =
      qobject_cast<ReosRainfallIntensityDurationCurveItem *>( ReosRainfallRegistery::instance()->itemByUniqueId( rainfall->intensityDurationUid() ) );
    if ( curveItem )
      mIdfWidget->setCurveItem( curveItem );
    else
      mIdfWidget->clearCurveItem();
  }

  connect( mIdfWidget, &ReosIntensityDurationSelectedCurveWidget::curveChanged, rainfall, [rainfall, this]
  {
    if ( mIdfWidget->curveItem() )
      rainfall->setIntensityDurationCurve( this->mIdfWidget->curveItem()->data(), this->mIdfWidget->curveItem()->uniqueId() );
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
      qobject_cast<ReosRainfallIntensityDurationCurveItem *>( ReosRainfallRegistery::instance()->itemByUniqueId( rainfall->intensityDurationUniqueIdIntense() ) );
    if ( intenseCurveItem )
      mIntenseIdfWidget->setCurveItem( intenseCurveItem );
    else
      mIntenseIdfWidget->clearCurveItem();

    ReosRainfallIntensityDurationCurveItem *totalCurveItem =
      qobject_cast<ReosRainfallIntensityDurationCurveItem *>( ReosRainfallRegistery::instance()->itemByUniqueId( rainfall->intensityDurationUniqueIdTotal() ) );
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
                                           this->mIntenseIdfWidget->curveItem()->uniqueId(),
                                           this->mTotalIdfWidget->curveItem()->uniqueId() );
  } );

  connect( mTotalIdfWidget, &ReosIntensityDurationSelectedCurveWidget::curveChanged, rainfall, [rainfall, this]
  {
    if ( this->mIntenseIdfWidget->curveItem() && this->mTotalIdfWidget->curveItem() )
      rainfall->setIntensityDurationCurve( this->mIntenseIdfWidget->curveItem()->data(),
                                           this->mTotalIdfWidget->curveItem()->data(),
                                           this->mIntenseIdfWidget->curveItem()->uniqueId(),
                                           this->mTotalIdfWidget->curveItem()->uniqueId() );
  } );

  addWidget( mIntenseIdfWidget, 4 );
  addWidget( mTotalIdfWidget, 4 );
  addParameter( rainfall->centerCoefficient() );
}


ReosFormWidget *ReosFormWidgetTimeSerieConstantIntervalFactory::createDataWidget( ReosDataObject *dataObject, QWidget *parent )
{
  ReosTimeSerieConstantInterval *object = qobject_cast<ReosTimeSerieConstantInterval *>( dataObject );
  if ( object )
    return new ReosTimeSerieConstantIntervalWidget( object, parent );

  return nullptr;
}


ReosFormWidget *ReosFormWidgetChicagoRainfalFactory::createDataWidget( ReosDataObject *dataObject, QWidget *parent )
{
  ReosChicagoRainfall *object = qobject_cast<ReosChicagoRainfall *>( dataObject );
  if ( object )
    return new ReosChicagoRainfallWidget( object, parent );

  return nullptr;
}

ReosFormWidget *ReosFormWidgetDoubleTriangleRainfalFactory::createDataWidget( ReosDataObject *dataObject, QWidget *parent )
{
  ReosDoubleTriangleRainfall *object = qobject_cast<ReosDoubleTriangleRainfall *>( dataObject );
  if ( object )
    return new ReosDoubleTriangleRainfallWidget( object, parent );

  return nullptr;
}

ReosFormWidget *ReosFormWidgetIntensityDurationCurveFactory::createDataWidget( ReosDataObject *dataObject, QWidget *parent )
{
  ReosIntensityDurationCurve *object = qobject_cast<ReosIntensityDurationCurve *>( dataObject );
  if ( object )
    return new ReosRainfallIntensityDurationWidget( object, parent );

  return nullptr;
}


ReosFormWidget *ReosFormWidgetAlternatingBlockRainfalFactory::createDataWidget( ReosDataObject *dataObject, QWidget *parent )
{
  ReosAlternatingBlockRainfall *object = qobject_cast<ReosAlternatingBlockRainfall *>( dataObject );
  if ( object )
    return new ReosAlternatingBlockRainfallWidget( object, parent );

  return nullptr;
}

ReosAlternatingBlockRainfallWidget::ReosAlternatingBlockRainfallWidget( ReosAlternatingBlockRainfall *rainfall, QWidget *parent ):
  ReosTimeSerieConstantIntervalWidget( rainfall, parent ),
  mIdfWidget( new ReosIntensityDurationSelectedCurveWidget( this ) )
{
  addParameter( rainfall->totalDuration(), 1 );

  if ( ReosRainfallRegistery::isInstantiate() )
  {
    ReosRainfallIntensityDurationCurveItem *curveItem =
      qobject_cast<ReosRainfallIntensityDurationCurveItem *>( ReosRainfallRegistery::instance()->itemByUniqueId( rainfall->intensityDurationUid() ) );
    if ( curveItem )
      mIdfWidget->setCurveItem( curveItem );
    else
      mIdfWidget->clearCurveItem();
  }

  connect( mIdfWidget, &ReosIntensityDurationSelectedCurveWidget::curveChanged, rainfall, [rainfall, this]
  {
    if ( mIdfWidget->curveItem() )
      rainfall->setIntensityDurationCurve( this->mIdfWidget->curveItem()->data(), this->mIdfWidget->curveItem()->uniqueId() );
  } );

  addWidget( mIdfWidget, 3 );
  addParameter( rainfall->centerCoefficient() );
}

ReosHorizontalHeaderView::ReosHorizontalHeaderView( QWidget *parent ): QHeaderView( Qt::Horizontal, parent )
{
  setSectionResizeMode( QHeaderView::ResizeToContents );
}

QSize ReosHorizontalHeaderView::sectionSizeFromContents( int logicalIndex ) const
{
  //https://stackoverflow.com/questions/45084542/qtableview-header-word-wrap
  const QString text = this->model()->headerData( logicalIndex, this->orientation(), Qt::DisplayRole ).toString();
  const int maxWidth = this->sectionSize( logicalIndex );
  const int maxHeight = 5000; // arbitrarily large
  const auto alignment = defaultAlignment();
  const QFontMetrics metrics( this->fontMetrics() );
  const QRect rect = metrics.boundingRect( QRect( 0, 0, maxWidth, maxHeight ), alignment, text );

  const QSize textMarginBuffer( 2, 2 ); // buffer space around text preventing clipping
  return rect.size() + textMarginBuffer;
}
