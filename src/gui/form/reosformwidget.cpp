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

ReosFormWidget::ReosFormWidget( QWidget *parent ) : QWidget( parent )
{
  setLayout( new QVBoxLayout( this ) );
  layout()->setContentsMargins( 0, 0, 0, 0 );
  layout()->setSpacing( 0 );
}

void ReosFormWidget::addText( const QString &text )
{
  layout()->addWidget( new QLabel( text ) );
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
    if ( p )
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

void ReosFormDialog::addText( const QString &text )
{
  mForm->addText( text );
}


ReosTimeSerieConstantIntervalWidget::ReosTimeSerieConstantIntervalWidget( ReosTimeSerieConstantInterval *timeSerie, QWidget *parent ):
  ReosFormWidget( parent )
  , mModel( new ReosTimeSerieConstantIntervalModel( this ) )
{
  mModel->setSerieData( timeSerie );
  addParameter( timeSerie->timeStep() );
  addParameter( timeSerie->referenceTime() );

  mValueModeComboBox = new QComboBox( this );
  mValueModeComboBox->addItem( timeSerie->valueModeName( ReosTimeSerieConstantInterval::Value ), ReosTimeSerieConstantInterval::Value );
  mValueModeComboBox->addItem( timeSerie->valueModeName( ReosTimeSerieConstantInterval::Intensity ), ReosTimeSerieConstantInterval::Intensity );

  layout()->addWidget( mValueModeComboBox );

  connect( mValueModeComboBox, QOverload<int>::of( &QComboBox::currentIndexChanged ), timeSerie, [timeSerie, this]()
  {
    ReosTimeSerieConstantInterval::ValueMode mode = static_cast<ReosTimeSerieConstantInterval::ValueMode>( this->mValueModeComboBox->currentData().toInt() );
    timeSerie->setValueMode( mode );
  } );

  ReosTimeSerieConstantIntervalView *view = new ReosTimeSerieConstantIntervalView( this );
  layout()->addWidget( view );
  view->setModel( mModel );
  view->horizontalHeader()->setStretchLastSection( true );
  view->horizontalHeader()->setCascadingSectionResizes( true );
  layout()->addWidget( view );

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
