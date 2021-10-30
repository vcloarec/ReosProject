/***************************************************************************
  reosgaugedhydrographwidget.cpp - ReosGaugedHydrographWidget

 ---------------------
 begin                : 24.10.2021
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
#include "reosgaugedhydrographwidget.h"
#include "ui_reosgaugedhydrographwidget.h"

#include <QMessageBox>

#include "reoswatershed.h"
#include "reoshydrograph.h"
#include "reosformwidget.h"
#include "reoshydrographeditingwidget.h"

ReosGaugedHydrographWidget::ReosGaugedHydrographWidget( QWidget *parent )
  : ReosActionWidget( parent )
  , ui( new Ui::ReosGaugedHydrographWidget )
  , mActionAddHydrograph( new QAction( tr( "Add Gauged Hydrograph" ), this ) )
  , mActionDeleteHydrograph( new QAction( tr( "Delete Current Hydrograph" ), this ) )
  , mActionRenameHydrograph( new QAction( tr( "Rename Current Hydrograph" ), this ) )
{
  ui->setupUi( this );
  setWindowFlag( Qt::Dialog );

  ReosFormWidgetFactories::instance()->addDataWidgetFactory( new ReosHydrographEditingWidgetFactory );

  ui->mWidgetToolBar->setLayout( new QHBoxLayout );
  ui->mWidgetToolBar->layout()->setContentsMargins( 0, 0, 0, 0 );
  QToolBar *toolBar = new QToolBar( ui->mWidgetToolBar );
  ui->mWidgetToolBar->layout()->addWidget( toolBar );

  mActionAddHydrograph = toolBar->addAction( QPixmap( QStringLiteral( ":/images/add.svg" ) ), tr( "Add Gauged Hydrograph" ), this, &ReosGaugedHydrographWidget::onAddHydrograph );
  mActionDeleteHydrograph = toolBar->addAction( QPixmap( QStringLiteral( ":/images/remove.svg" ) ), tr( "Delete Current Hydrograph" ), this, &ReosGaugedHydrographWidget::onRemoveHydrograph );
  mActionRenameHydrograph = toolBar->addAction( QPixmap( QStringLiteral( ":/images/rename.svg" ) ), tr( "Rename Current Hydrograph" ), this, &ReosGaugedHydrographWidget::onRenameHydrograph );

  connect( ui->mComboBoxHydrographName, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosGaugedHydrographWidget::onCurrentHydrographChanged );

  onStoreChanged();
}

ReosGaugedHydrographWidget::~ReosGaugedHydrographWidget()
{
  delete ui;
}

void ReosGaugedHydrographWidget::setCurrentWatershed( ReosWatershed *watershed )
{
  mCurrentWatershed = watershed;

  if ( watershed )
  {
    mHydrographStore = watershed->gaugedHydrographs();
  }

  onStoreChanged();
  onCurrentHydrographChanged();
}

void ReosGaugedHydrographWidget::onAddHydrograph()
{
  ReosFormDialog *dial = new ReosFormDialog( this );
  dial->setWindowTitle( tr( "Add Hydrograph" ) );
  dial->addText( tr( "Add a new hydrograph, choose a name:" ) );
  ReosParameterString nameParam( tr( "Hydrograph name" ) );
  dial->addParameter( &nameParam );

  if ( dial->exec() )
  {
    std::unique_ptr<ReosHydrograph> newHydrograph = std::make_unique<ReosHydrograph>();
    newHydrograph->setName( nameParam.value() );
    mHydrographStore->addHydrograph( newHydrograph.release() );
    onStoreChanged();
  }

  dial->deleteLater();
}

void ReosGaugedHydrographWidget::onRemoveHydrograph()
{
  if ( !mHydrographStore )
    return;

  int currentIndex = ui->mComboBoxHydrographName->currentIndex();
  ReosHydrograph *hydrographToReMove = mHydrographStore->hydrograph( currentIndex );
  if ( !hydrographToReMove )
    return;

  if ( QMessageBox::warning( this, tr( "Remove Gauged Hydrograph" ),
                             tr( "Do you want to remove the hydrograph '%1'?" ).arg( hydrographToReMove->name() ),
                             QMessageBox::Yes | QMessageBox::No, QMessageBox::No ) == QMessageBox::Yes )
  {
    mHydrographStore->removeHydrograph( currentIndex );
    ui->mComboBoxHydrographName->removeItem( currentIndex );
  }
}

void ReosGaugedHydrographWidget::onRenameHydrograph()
{
  if ( !mHydrographStore )
    return;

  int currentIndex = ui->mComboBoxHydrographName->currentIndex();
  ReosHydrograph *hydrographToRename = mHydrographStore->hydrograph( currentIndex );
  if ( !hydrographToRename )
    return;

  ReosFormDialog *dial = new ReosFormDialog( this );
  dial->setWindowTitle( tr( "Rename Meteo Model" ) );
  dial->addText( tr( "Rename meteorologic model '%1',\nchoose a new name:" ).arg( hydrographToRename->name() ) );
  ReosParameterString nameParam( tr( "Meteorologic Model name" ) );
  nameParam.setValue( hydrographToRename->name() );
  dial->addParameter( &nameParam );

  if ( dial->exec() )
  {
    hydrographToRename->setName( nameParam.value() );
    ui->mComboBoxHydrographName->setItemText( currentIndex, nameParam.value() );
  }

  dial->deleteLater();
}

void ReosGaugedHydrographWidget::onStoreChanged()
{
  ui->mComboBoxHydrographName->clear();
  bool hasHydropraph = false;
  if ( mHydrographStore )
  {
    const QStringList &names = mHydrographStore->hydrographNames();
    ui->mComboBoxHydrographName->addItems( names );
    hasHydropraph = !names.isEmpty();
    mActionAddHydrograph->setEnabled( true );
  }
  else
  {
    mActionAddHydrograph->setEnabled( false );
  }

  mActionDeleteHydrograph->setEnabled( hasHydropraph );
  mActionRenameHydrograph->setEnabled( hasHydropraph );
}

void ReosGaugedHydrographWidget::onCurrentHydrographChanged()
{
  ReosHydrograph *currentHydrograph = nullptr;

  if ( mHydrographStore && mHydrographStore->hydrographCount() > 0 )
  {
    currentHydrograph = mHydrographStore->hydrograph( ui->mComboBoxHydrographName->currentIndex() );
  }

  std::unique_ptr<QWidget> newWidget;

  if ( currentHydrograph )
    newWidget.reset( ReosFormWidgetFactories::instance()->createDataFormWidget( currentHydrograph ) );
  else
    newWidget.reset( new QLabel( tr( "No Hydrograph" ) ) );

  if ( ui->mEditingWidgetLayout->count() != 0 )
  {
    ui->mEditingWidgetLayout->removeWidget( mCurrenEditingWidget );
    delete mCurrenEditingWidget;
  }

  mCurrenEditingWidget = newWidget.get();
  ui->mEditingWidgetLayout->addWidget( newWidget.get() );
  newWidget.release();
}
