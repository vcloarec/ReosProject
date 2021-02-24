/***************************************************************************
  reosrainfallintensitydurationwidget.cpp - ReosRainfallIntensityDurationWidget

 ---------------------
 begin                : 4.2.2021
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
#include "reosrainfallintensitydurationwidget.h"

#include "reosidfcurves.h"

#include <memory>

#include <QComboBox>
#include <QHeaderView>
#include <QMenu>
#include <QMessageBox>
#include <QMouseEvent>
#include <QLayout>

ReosRainfallIntensityDurationWidget::ReosRainfallIntensityDurationWidget( ReosIntensityDurationCurve *curve, QWidget *parent ):
  ReosFormWidget( parent, Qt::Vertical, false )
  , mComboFormula( new QComboBox( this ) )
  , mModel( new ReosIntensityDurationCurveTableModel( curve, this ) )
  , mView( new QTableView( this ) )
{
  mComboFormula->addItems( ReosIdfFormulaRegistery::instance()->formulasList() );
  mComboFormula->setCurrentText( curve->currentFormula() );
  layout()->addWidget( mComboFormula );

  mView->setModel( mModel );
  mView->horizontalHeader()->setStretchLastSection( true );
  mView->horizontalHeader()->setCascadingSectionResizes( true );
  mView->setContextMenuPolicy( Qt::CustomContextMenu );
  mView->verticalHeader()->setContextMenuPolicy( Qt::CustomContextMenu );
  layout()->addWidget( mView );

  connect( mComboFormula, &QComboBox::currentTextChanged, mModel, &ReosIntensityDurationCurveTableModel::setCurrentFormula );
  connect( mView->verticalHeader(), &QHeaderView::sectionDoubleClicked, this, &ReosRainfallIntensityDurationWidget::onVerticalHeaderDoubleClicked );

  connect( mView, &QWidget::customContextMenuRequested, this, &ReosRainfallIntensityDurationWidget::onTableViewContextMenu );
  connect( mView->verticalHeader(), &QWidget::customContextMenuRequested, this,  &ReosRainfallIntensityDurationWidget::onVerticalHeaderViewContextMenu );

  mModel->setCurrentFormula( mComboFormula->currentText() );

}

void ReosRainfallIntensityDurationWidget::onVerticalHeaderDoubleClicked( int section )
{

  QString text;

  std::unique_ptr<ReosParameterDuration> startParameter = std::make_unique<ReosParameterDuration>( tr( "Interval duration start:" ) );
  std::unique_ptr<ReosParameterDuration> endParameter = std::make_unique<ReosParameterDuration>( tr( "Interval duration start:" ) );
  bool newInterval = section == mModel->rowCount( QModelIndex() ) - 1;

  if ( newInterval )
  {
    text = tr( "Add a new interval" );
    if ( mModel->curve()->intervalCount() > 0 )
    {
      startParameter->setValue( mModel->curve()->timeInterval( mModel->curve()->intervalCount() - 1 ).second );
      endParameter->setValue( startParameter->value() * 2 );
    }
    else
    {
      startParameter->setValue( ReosDuration( 5, ReosDuration::minute ) );
      endParameter->setValue( ReosDuration( 15, ReosDuration::minute ) );
    }
  }
  else
  {
    startParameter->setValue( mModel->curve()->timeInterval( section ).first );
    endParameter->setValue( mModel->curve()->timeInterval( section ).second );
  }

  ReosFormDialog *dial = new ReosFormDialog( this );
  dial->addText( text );
  dial->addParameter( startParameter.get() );
  dial->addParameter( endParameter.get() );

  if ( ! dial->exec() )
    return;

  bool result = false;
  if ( newInterval )
    result = mModel->curve()->addInterval( startParameter->value(), endParameter->value() ) ;
  else
    result = mModel->curve()->setIntervalValue( section, startParameter->value(), endParameter->value() );

  if ( !result )
    QMessageBox::warning( this, text, "Invalid interval parameters" );
}


void ReosRainfallIntensityDurationWidget::onTableViewContextMenu( const QPoint &pos )
{
  QModelIndex index = mView->indexAt( pos );
  contextMenu( mView->viewport()->mapToGlobal( pos ), index );
}

void ReosRainfallIntensityDurationWidget::onVerticalHeaderViewContextMenu( const QPoint &pos )
{
  QModelIndex index = mView->indexAt( pos );
  contextMenu( mView->verticalHeader()->mapToGlobal( pos ), index );
}

void ReosRainfallIntensityDurationWidget::contextMenu( const QPoint &globalPos, const QModelIndex &index )
{
  int row = index.row();

  if ( row >= 0 && row < mModel->curve()->intervalCount() )
  {
    QMenu contextMenu;
    contextMenu.addAction( tr( "Remove interval" ), &contextMenu, [this, row]
    {
      if ( QMessageBox::warning( this, tr( "Remove interval" ), tr( "Do you want to remove the interval?" ), QMessageBox::No | QMessageBox::Yes, QMessageBox::No ) ==
           QMessageBox::Yes )
      {
        mModel->curve()->removeInterval( row );
      }

    } );

    contextMenu.exec( globalPos );
  }
}

