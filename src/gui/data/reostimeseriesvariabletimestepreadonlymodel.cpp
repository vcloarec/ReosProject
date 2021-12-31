/***************************************************************************
  reostimeseriesvariabletimestepreadonlymodel.cpp - ReosTimeSeriesVariableTimeStepReadOnlyModel

 ---------------------
 begin                : 21.12.2021
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
#include "reostimeseriesvariabletimestepreadonlymodel.h"

#include <QPointer>
#include <QHeaderView>
#include <QVBoxLayout>
#include <QCheckBox>
#include <QMenu>
#include <QItemSelection>
#include <QApplication>
#include <QClipboard>

#include "reostimeserie.h"
#include "reosparameterwidget.h"
#include "reosparameter.h"
#include "reostableview.h"


QModelIndex ReosTimeSeriesVariableTimeStepReadOnlyModel::index( int row, int column, const QModelIndex & ) const
{
  return createIndex( row, column );
}

QModelIndex ReosTimeSeriesVariableTimeStepReadOnlyModel::parent( const QModelIndex & ) const
{
  return QModelIndex();
}

int ReosTimeSeriesVariableTimeStepReadOnlyModel::rowCount( const QModelIndex & ) const
{
  if ( mTimeSeries.isEmpty() )
    return 0;

  if ( isFixedTimeStep() )
  {
    return mTimeStepCount;
  }

  return mTimeSeries.at( 0 )->valueCount();
}

int ReosTimeSeriesVariableTimeStepReadOnlyModel::columnCount( const QModelIndex & ) const
{
  if ( mTimeSeries.isEmpty() )
    return 0;

  return mTimeSeries.count() + 1;
}

QVariant ReosTimeSeriesVariableTimeStepReadOnlyModel::data( const QModelIndex &index, int role ) const
{
  if ( !index.isValid() )
    return QVariant();

  if ( index.row() >= rowCount( QModelIndex() ) )
    return QVariant();

  if ( columnCount( QModelIndex() ) <= 0 )
    return QVariant();

  int row = index.row();

  if ( role == Qt::DisplayRole )
  {
    switch ( index.column() )
    {
      case 0: //time
        return timeAtRow( row ).toString( QLocale().dateTimeFormat( QLocale::ShortFormat ) );
        break;
      default:
        return valueAt( row, index.column() );
        break;;
    }
  }

  if ( role == Qt::TextAlignmentRole )
  {
    return Qt::AlignHCenter;
  }

  return QVariant();
}

QVariant ReosTimeSeriesVariableTimeStepReadOnlyModel::headerData( int section, Qt::Orientation orientation, int role ) const
{
  if ( orientation == Qt::Vertical )
    return QVariant();

  if ( section > mTimeSeries.count() )
    return QVariant();

  if ( role == Qt::DisplayRole )
  {
    switch ( section )
    {
      case 0: //time
        return tr( "Time" );
        break;
      default:
        return mHeaderName.at( section - 1 );
        break;
    }
  }

  return QVariant();
}

void ReosTimeSeriesVariableTimeStepReadOnlyModel::addTimeSerie( ReosTimeSerieVariableTimeStep *timeSerie, const QString &name )
{
  beginResetModel();
  mTimeSeries.append( timeSerie );
  mHeaderName.append( name );
  endResetModel();
  connect( timeSerie, &ReosDataObject::dataChanged, this, &ReosTimeSeriesVariableTimeStepReadOnlyModel::updateTimeStep );
  connect( timeSerie, &ReosDataObject::dataChanged, this, [this]
  {
    beginResetModel();
    emit hasBeenReset();
    endResetModel();
  } );
  updateTimeStep();
  emit hasBeenReset();

}

void ReosTimeSeriesVariableTimeStepReadOnlyModel::clearSerie()
{
  beginResetModel();
  for ( int i = 0; i < mTimeSeries.count(); i++ )
  {
    if ( mTimeSeries.at( i ).isNull() )
      continue;
    disconnect( mTimeSeries.at( i ).data(), &ReosDataObject::dataChanged, this, &ReosTimeSeriesVariableTimeStepReadOnlyModel::updateTimeStep );
  }
  mTimeSeries.clear();
  mHeaderName.clear();
  endResetModel();
  emit hasBeenReset();
}

void ReosTimeSeriesVariableTimeStepReadOnlyModel::updateTimeStep()
{
  if ( !isFixedTimeStep() )
    return;
  beginResetModel();

  QDateTime firstTime;
  QDateTime lastTime;

  for ( int i = 0; i < mTimeSeries.count(); ++i )
  {
    if ( mTimeSeries.at( i ).isNull() )
      continue;
    ReosTimeSerieVariableTimeStep *serie = mTimeSeries.at( i );
    if ( serie->valueCount() == 0 )
      continue;
    QDateTime begin = serie->timeAt( 0 );
    QDateTime end = serie->timeAt( serie->valueCount() - 1 );

    if ( !firstTime.isValid() || ( begin.isValid() && firstTime > begin ) )
    {
      firstTime = begin;
      mFirstSerieIndex = i;
    }

    if ( !lastTime.isValid() || ( end.isValid() && lastTime < end ) )
    {
      lastTime = end;
      mLastSerieIndex = i;
    }
  }

  mFirstTime = firstTime;
  mTimeStepCount = ReosDuration( firstTime.msecsTo( lastTime ) ) / mTimeStep + 1;

  endResetModel();
  emit hasBeenReset();
}

ReosDuration ReosTimeSeriesVariableTimeStepReadOnlyModel::timeStep() const
{
  return mTimeStep;
}

void ReosTimeSeriesVariableTimeStepReadOnlyModel::setTimeStep( const ReosDuration &timeStep )
{
  beginResetModel();
  mTimeStep = timeStep;
  endResetModel();
  updateTimeStep();
  emit hasBeenReset();
}

void ReosTimeSeriesVariableTimeStepReadOnlyModel::copyResultHydrographValues( QItemSelectionModel *selectionModel, bool withHeader )
{
  if ( !selectionModel )
    return;

  const QItemSelection &selection = selectionModel->selection();

  QString copyText;
  if ( !selection.isEmpty() )
  {
    QStringList lines;
    const QItemSelectionRange &range = selection.first();
    if ( withHeader )
    {
      QStringList headers;
      // headers
      for ( int w = 0; w < range.width(); ++w )
      {
        QString header =   headerData( range.left() + w, Qt::Horizontal, Qt::DisplayRole ).toString();
        header.replace( QStringLiteral( "\n" ), QString( ' ' ) );
        headers.append( header );
      }
      lines.append( headers.join( QStringLiteral( "\t" ) ) );
    }
    for ( int h = 0; h < range.height(); ++h )
    {
      QStringList lineData;
      for ( int w = 0; w < range.width(); ++w )
        lineData.append( data(
                           index( range.top() + h, range.left() + w, QModelIndex() ), Qt::DisplayRole ).toString() );

      lines.append( lineData.join( QStringLiteral( "\t" ) ) );
    }
    copyText = lines.join( QStringLiteral( "\n" ) );
  }

  QApplication::clipboard()->setText( copyText );
}

QDateTime ReosTimeSeriesVariableTimeStepReadOnlyModel::timeAtRow( int row ) const
{
  if ( isFixedTimeStep() )
    return mFirstTime.addMSecs( ( mTimeStep * row ).valueMilliSecond() );

  if ( !mTimeSeries.at( 0 ).isNull() && row < mTimeSeries.at( 0 )->valueCount() )
    return mTimeSeries.at( 0 )->timeAt( row );

  return QDateTime();
}

QVariant ReosTimeSeriesVariableTimeStepReadOnlyModel::valueAt( int row, int column ) const
{
  ReosTimeSerieVariableTimeStep *serie = mTimeSeries.at( column - 1 );
  if ( !serie || serie->valueCount() == 0 )
    return QVariant();

  if ( isFixedTimeStep() )
  {
    ReosDuration relativeTime( serie->referenceTime().msecsTo( mFirstTime ) );
    return serie->valueAtTime( relativeTime + mTimeStep * row );
  }

  QDateTime dateTime;

  if ( row < mTimeSeries.at( 0 )->valueCount() )
    dateTime = mTimeSeries.at( 0 )->referenceTime().addMSecs( mTimeSeries.at( 0 )->relativeTimeAt( row ).valueMilliSecond() );

  if ( dateTime >= serie->timeAt( 0 ) && dateTime <= serie->timeAt( serie->valueCount() - 1 ) )
    return serie->valueAtTime( dateTime );

  return QVariant();
}

bool ReosTimeSeriesVariableTimeStepReadOnlyModel::isFixedTimeStep() const
{
  return mIsFixedTimeStep;
}

void ReosTimeSeriesVariableTimeStepReadOnlyModel::setIsFixedTimeStep( bool isFixedTimeStep )
{
  beginResetModel();
  mIsFixedTimeStep = isFixedTimeStep;
  endResetModel();
  updateTimeStep();
  emit hasBeenReset();
}

ReosTimeSeriesVariableTimeStepReadOnlyTableView::ReosTimeSeriesVariableTimeStepReadOnlyTableView( QWidget *parent ) : QTableView( parent )
{
  setHorizontalHeader( new ReosHorizontalHeaderView( this ) );
  horizontalHeader()->setStretchLastSection( true );
  setContextMenuPolicy( Qt::CustomContextMenu );
  setVerticalScrollBarPolicy( Qt::ScrollBarAlwaysOn );
  setHorizontalScrollBarPolicy( Qt::ScrollBarAlwaysOff );
  setSizeAdjustPolicy( QTableView::AdjustToContents );
  setSelectionMode( QAbstractItemView::ContiguousSelection );

  connect( this, &QWidget::customContextMenuRequested, this, &ReosTimeSeriesVariableTimeStepReadOnlyTableView::onContextMenu );
}

void ReosTimeSeriesVariableTimeStepReadOnlyTableView::setTableModel( ReosTimeSeriesVariableTimeStepReadOnlyModel *model )
{
  mModel = model;
  setModel( model );
  connect( mModel, &ReosTimeSeriesVariableTimeStepReadOnlyModel::hasBeenReset, this, [this]
  {
    horizontalHeader()->resizeSections( QHeaderView::ResizeToContents );
  } );
}

void ReosTimeSeriesVariableTimeStepReadOnlyTableView::onContextMenu( const QPoint &pos )
{
  QMenu contextMenu;

  contextMenu.addAction( tr( "Copy selected values" ), &contextMenu, [this]
  {
    this->copySelected( false );
  } );

  contextMenu.addAction( tr( "Copy selected values with headers" ), &contextMenu, [this]
  {
    this->copySelected( true );
  } );

  contextMenu.exec( mapToGlobal( pos ) );
}

void ReosTimeSeriesVariableTimeStepReadOnlyTableView::copySelected( bool withHeader )
{
  QItemSelectionModel *selecModel = selectionModel();
  mModel->copyResultHydrographValues( selecModel, withHeader );
}

ReosTimeSeriesVariableTimeStepReadOnlyTablesView::ReosTimeSeriesVariableTimeStepReadOnlyTablesView( QWidget *parent ) : QWidget( parent )
{
  QVBoxLayout *mainLayout = new QVBoxLayout;
  setLayout( mainLayout );

  QHBoxLayout *tableLayout = new QHBoxLayout;
  tableLayout->setContentsMargins( 0, 0, 0, 0 );
  tableLayout->addSpacerItem( new QSpacerItem( 20, 0, QSizePolicy::Preferred ) );

  mView = new ReosTimeSeriesVariableTimeStepReadOnlyTableView( this );
  tableLayout->addWidget( mView );
  tableLayout->addSpacerItem( new QSpacerItem( 20, 0, QSizePolicy::Preferred ) );
  tableLayout->setStretch( 0, 1 );
  tableLayout->setStretch( 2, 1 );
  mainLayout->addItem( tableLayout );

  mModel = new ReosTimeSeriesVariableTimeStepReadOnlyModel( this );
  mView->setTableModel( mModel );

  mTimeStepLayout = new QHBoxLayout;
  mTimeStepWidget = new ReosParameterDurationWidget( this );
  mConstantTimeStepWidget = new ReosParameterBooleanWidget( this );
  mTimeStepLayout->addWidget( mConstantTimeStepWidget );
  mTimeStepLayout->addWidget( mTimeStepWidget );
  mTimeStepLayout->addSpacerItem( new QSpacerItem( 40, 0 ) );
  mTimeStepLayout->setStretch( 2, 1 );
  mainLayout->addLayout( mTimeStepLayout );

  mainLayout->setStretch( 0, 1 );
}

void ReosTimeSeriesVariableTimeStepReadOnlyTablesView::setConstantTimeStepParameter( ReosParameterDuration *constantTimeSepParameter, ReosParameterBoolean *useConstantTimeStepParameter )
{
  mModel->setIsFixedTimeStep( useConstantTimeStepParameter->value() );
  mModel->setTimeStep( constantTimeSepParameter->value() );

  mTimeStepWidget->setDuration( constantTimeSepParameter );
  mConstantTimeStepWidget->setBooleanParameter( useConstantTimeStepParameter );

  mTimeStepWidget->setVisible( useConstantTimeStepParameter->value() );

  connect( constantTimeSepParameter, &ReosParameterDuration::valueChanged, mModel, [this, constantTimeSepParameter]
  {
    mModel->setTimeStep( constantTimeSepParameter->value() );
  } );

  connect( useConstantTimeStepParameter, &ReosParameterBoolean::valueChanged, mModel, [this, useConstantTimeStepParameter]
  {
    mModel->setIsFixedTimeStep( useConstantTimeStepParameter->value() );
    mTimeStepWidget->setVisible( useConstantTimeStepParameter->value() );
  } );
}

void ReosTimeSeriesVariableTimeStepReadOnlyTablesView::setSeries( QList < ReosTimeSerieVariableTimeStep *> series, const QString &unit )
{
  for ( ReosTimeSerieVariableTimeStep *serie : std::as_const( series ) )
    mModel->addTimeSerie( serie, tr( "%1\n(%2)" ).arg( serie->name(), unit ) );

  mView->horizontalHeader()->resizeSections( QHeaderView::ResizeToContents );
}

void ReosTimeSeriesVariableTimeStepReadOnlyTablesView::clearSeries()
{
  mModel->clearSerie();
}
