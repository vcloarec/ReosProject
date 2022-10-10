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
#include <QLabel>
#include <QMenu>
#include <QMessageBox>
#include <QMouseEvent>
#include <QLayout>
#include <QToolButton>

ReosRainfallIntensityDurationWidget::ReosRainfallIntensityDurationWidget( ReosIntensityDurationCurve *curve, QWidget *parent ):
  ReosFormWidget( parent, Qt::Vertical, false )
  , mComboFormula( new QComboBox( this ) )
  , mButtonDisplayFormula( new QToolButton( this ) )
  , mModel( new ReosIntensityDurationCurveTableModel( curve, this ) )
  , mView( new QTableView( this ) )
{
  QFrame *line = new QFrame( this );
  line->setFrameShape( QFrame::HLine );
  line->setFrameShadow( QFrame::Sunken );
  layout()->addWidget( line );
  QHBoxLayout *comboFormulaLayout = new QHBoxLayout;
  mComboFormula->addItems( ReosIdfFormulaRegistery::instance()->formulasList() );
  mComboFormula->setCurrentText( curve->currentFormula() );
  comboFormulaLayout->addWidget( mComboFormula );
  mButtonDisplayFormula->setIcon( QIcon( QStringLiteral( ":/images/function.svg" ) ) );
  comboFormulaLayout->addWidget( mButtonDisplayFormula );
  layout()->addItem( comboFormulaLayout );

  QHBoxLayout *layoutParameterTimeUnit = new QHBoxLayout;
  layoutParameterTimeUnit->addWidget( new QLabel( tr( "Entry time unit" ), this ) );
  layoutParameterTimeUnit->addItem( new QSpacerItem( 5, 0, QSizePolicy::Minimum, QSizePolicy::Ignored ) );
  mParameterTimeUnitComboBox = new ReosDurationUnitComboBox( this );
  layoutParameterTimeUnit->addWidget( mParameterTimeUnitComboBox );
  layout()->addItem( layoutParameterTimeUnit );

  QHBoxLayout *layoutResultTimeUnit = new QHBoxLayout;
  layoutResultTimeUnit->addWidget( new QLabel( tr( "Result time unit" ), this ) );
  layoutResultTimeUnit->addItem( new QSpacerItem( 5, 0, QSizePolicy::Minimum, QSizePolicy::Ignored ) );
  mResultTimeUnitComboBox = new ReosDurationUnitComboBox( this, ReosDuration::hour );
  layoutResultTimeUnit->addWidget( mResultTimeUnitComboBox );
  layout()->addItem( layoutResultTimeUnit );

  mView->setModel( mModel );
  mView->horizontalHeader()->setSectionResizeMode( QHeaderView::Stretch );
  mView->setContextMenuPolicy( Qt::CustomContextMenu );
  mView->verticalHeader()->setContextMenuPolicy( Qt::CustomContextMenu );
  layout()->addWidget( mView );

  connect( mComboFormula, &QComboBox::currentTextChanged, mModel, &ReosIntensityDurationCurveTableModel::setCurrentFormula );
  connect( mView->verticalHeader(), &QHeaderView::sectionDoubleClicked, this, &ReosRainfallIntensityDurationWidget::onVerticalHeaderDoubleClicked );

  connect( mView, &QWidget::customContextMenuRequested, this, &ReosRainfallIntensityDurationWidget::onTableViewContextMenu );
  connect( mView->verticalHeader(), &QWidget::customContextMenuRequested, this,  &ReosRainfallIntensityDurationWidget::onVerticalHeaderViewContextMenu );

  connect( mParameterTimeUnitComboBox, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosRainfallIntensityDurationWidget::onParameterTimeUnitChanged );
  connect( mResultTimeUnitComboBox, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosRainfallIntensityDurationWidget::onResultTimeUnitChanged );

  mModel->setCurrentFormula( mComboFormula->currentText() );
  if ( curve )
  {
    mParameterTimeUnitComboBox->setCurrentUnit( curve->currentParameterTimeUnit() ) ;
    mResultTimeUnitComboBox->setCurrentUnit( curve->currentResultTimeUnit() );
  }

  connect( mButtonDisplayFormula, &QToolButton::clicked, this, &ReosRainfallIntensityDurationWidget::onDisplayingFormula );

}

void ReosRainfallIntensityDurationWidget::onVerticalHeaderDoubleClicked( int section )
{

  QString text;

  std::unique_ptr<ReosParameterDuration> startParameter = std::make_unique<ReosParameterDuration>( tr( "Interval duration start:" ) );
  std::unique_ptr<ReosParameterDuration> endParameter = std::make_unique<ReosParameterDuration>( tr( "Interval duration end:" ) );
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
  dial->setWindowTitle( tr( "Duration Interval Edition" ) );
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

void ReosRainfallIntensityDurationWidget::onParameterTimeUnitChanged()
{
  if ( mModel->curve() )
    mModel->curve()->setCurrentParameterTimeUnit( mParameterTimeUnitComboBox->currentUnit() );

}

void ReosRainfallIntensityDurationWidget::onResultTimeUnitChanged()
{
  if ( mModel->curve() )
    mModel->curve()->setCurrentResultTimeUnit( mResultTimeUnitComboBox->currentUnit() );
}

void ReosRainfallIntensityDurationWidget::onDisplayingFormula()
{
  if ( !ReosIdfFormulaRegistery::isInstantiate() )
    return;

  ReosIdfFormulaRegistery *registery = ReosIdfFormulaRegistery::instance();

  QDialog *dial = new QDialog( this );

  dial->setWindowTitle( mComboFormula->currentText() );
  dial->setLayout( new QVBoxLayout );

  QLabel *formulaLabel = new QLabel( this );
  formulaLabel->setAlignment( Qt::AlignHCenter );
  formulaLabel->setPixmap( registery->formula( mComboFormula->currentText() )->formulaImage() );
  dial->layout()->addWidget( formulaLabel );

  QFrame *line = new QFrame( this );
  line->setFrameShape( QFrame::HLine );
  line->setFrameShadow( QFrame::Sunken );
  dial->layout()->addWidget( line );

  dial->layout()->addWidget( new QLabel( tr( "I: rainfall intensity (mm per result time unit)" ) ) );
  dial->layout()->addWidget( new QLabel( tr( "t: rainfall duration (entry time unit)" ) ) );

  dial->exec();

  dial->deleteLater();
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

