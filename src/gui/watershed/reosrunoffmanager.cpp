/***************************************************************************
  reosrunoffmanager.cpp - ReosRunoffManager

 ---------------------
 begin                : 21.2.2021
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
#include "reosrunoffmanager.h"
#include "ui_reosrunoffmanager.h"

#include <QMenu>
#include <QMessageBox>
#include <QFileDialog>

#include "reosrunoffmodel.h"
#include "reosformwidget.h"
#include "reosparameter.h"
#include "reossettings.h"
#include "reostimeserie.h"

ReosRunoffManager::ReosRunoffManager( ReosRunoffModelModel *model, QWidget *parent ) :
  ReosActionWidget( parent ),
  ui( new Ui::ReosRunoffManager )
  , mRunoffModelModel( model )

{
  ui->setupUi( this );
  setWindowFlag( Qt::Dialog );
  ui->treeView->setModel( model );

  ui->treeView->setContextMenuPolicy( Qt::CustomContextMenu );

  QToolBar *toolBar = new QToolBar( this );
  ui->widgetTools->layout()->addWidget( toolBar );
  toolBar->addAction( QPixmap( QStringLiteral( ":/images/openRunoff.svg" ) ), tr( "Open File" ), this, &ReosRunoffManager::onOpenFile );
  toolBar->addAction( QPixmap( QStringLiteral( ":/images/saveRunoff.svg" ) ), tr( "Save Runoff Models" ), this, &ReosRunoffManager::onSave );
  toolBar->addAction( QPixmap( QStringLiteral( ":/images/saveRunoffAs.svg" ) ), tr( "Save Runoff Models as ..." ), this, &ReosRunoffManager::onSaveAs );

  connect( ui->treeView->selectionModel(), &QItemSelectionModel::selectionChanged, this, &ReosRunoffManager::onCurrentTreeIndexChanged );
  onCurrentTreeIndexChanged();

  connect( ui->treeView, &QWidget::customContextMenuRequested, this, &ReosRunoffManager::onTreeViewContextMenu );
}

ReosRunoffManager::~ReosRunoffManager()
{
  delete ui;
}

void ReosRunoffManager::loadDataFile()
{
  if ( !ReosRunoffModelRegistery::isInstantiate() )
    return;

  ReosRunoffModelRegistery *registery = ReosRunoffModelRegistery::instance();

  QString fileName;
  ReosSettings settings;
  fileName = settings.value( QStringLiteral( "Runoff-model/dataFile" ) ).toString();

  if ( fileName.isEmpty() )
    return;

  if ( registery->loadFromFile( fileName, QStringLiteral( "rainfall data" ) ) )
  {
    mCurrentFile = fileName;
  }
  else
  {
    QMessageBox::warning( this, tr( "Open Runoff Data" ), tr( "Unable to open the current runoff data file: %1" ).arg( fileName ) );
  }
}

void ReosRunoffManager::onSave()
{
  QFileInfo fileInfo( mCurrentFile );
  if ( !fileInfo.exists() )
  {
    onSaveAs();
    return;
  }

  if ( !saveOn( mCurrentFile ) )
    QMessageBox::warning( this, tr( "Save Rainfall Data" ), tr( "Unable to write the file" ) );
}

void ReosRunoffManager::onSaveAs()
{
  ReosSettings settings;
  QString dir = settings.value( QStringLiteral( "Runoff-model/fileDirectory" ) ).toString();
  QString fileName = QFileDialog::getSaveFileName( this, tr( "Save Runoff Data as..." ), dir, QStringLiteral( " *.rro" ) );

  if ( fileName.isEmpty() )
    return;

  QFileInfo fileInfo( fileName );
  if ( fileInfo.suffix().isEmpty() )
    fileName.append( QStringLiteral( ".rro" ) );

  if ( !saveOn( fileName ) )
    QMessageBox::warning( this, tr( "Save Runoff Data as..." ), tr( "Unable to write the file" ) );
  else
  {
    mCurrentFile = fileName;
    settings.setValue( QStringLiteral( "Runoff-model/fileDirectory" ), fileInfo.path() );
  }
}

void ReosRunoffManager::onOpenFile()
{
  if ( !ReosRunoffModelRegistery::isInstantiate() )
    return;

  ReosRunoffModelRegistery *registery = ReosRunoffModelRegistery::instance();

  if ( mRunoffModelModel->hasData() )
  {
    int ret = QMessageBox::warning( this, tr( "Open Runoff Data File" ),
                                    tr( "This action will remove the actual runoff model data, do you want to save before?" ),
                                    QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel );

    if ( ret == QMessageBox::Cancel )
      return;

    if ( ret == QMessageBox::Yes )
    {
      onSave();
    }
  }

  ReosSettings settings;
  QString dir = settings.value( QStringLiteral( "Runoff-model/fileDirectory" ) ).toString();
  QString fileName = QFileDialog::getOpenFileName( this, tr( "Open Runoff Data" ), dir, QStringLiteral( " *.rro" ) );

  if ( fileName.isEmpty() )
    return;

  if ( registery->loadFromFile( fileName, QStringLiteral( "runoff data" ) ) )
  {
    mCurrentFile = fileName;
    settings.setValue( QStringLiteral( "Runoff-model/dataFile" ), fileName );
    QFileInfo fileInfo( fileName );
    settings.setValue( QStringLiteral( "Runoff-model/fileDirectory" ), fileInfo.path() );
  }
  else
  {
    QMessageBox::critical( this, tr( "Open Runoff Data" ), tr( "Unable to open the file: %1" ).arg( fileName ) );
  }

  onCurrentTreeIndexChanged();
}

void ReosRunoffManager::onAddNewModel( const QString &type )
{
  if ( !ReosRunoffModelRegistery::isInstantiate() )
    return;

  ReosRunoffModelRegistery *registery = ReosRunoffModelRegistery::instance();

  ReosFormDialog *dial = new ReosFormDialog( this );

  dial->setWindowTitle( tr( "Add a nw runoff model" ) );
  dial->addText( tr( "Choose model's name:" ) );
  ReosParameterString *name = new ReosParameterString( tr( "Name" ), false, this );
  name->setValue( registery->createRunoffModelName( type ) );
  dial->addParameter( name );

  if ( dial->exec() )
    selectRunoffModel( registery->createModel( type, name->value() ) );

  dial->deleteLater();
}

void ReosRunoffManager::onRemoveRunoffModel( ReosRunoffModel *runoffModel )
{
  if ( !runoffModel )
    return;

  if ( QMessageBox::warning( this, tr( "Remove runoff model" ),
                             tr( "Remove runoff model %1?" ).arg( runoffModel->name()->value() ),
                             QMessageBox::Yes | QMessageBox::No, QMessageBox::No ) == QMessageBox::Yes )
    mRunoffModelModel->removeRunoffModel( runoffModel );
}

void ReosRunoffManager::onCurrentTreeIndexChanged()
{
  QModelIndex currentIndex = ui->treeView->currentIndex();

  ReosRunoffModel *runoffModel = mRunoffModelModel->runoffModel( currentIndex );
  QString runoffType = mRunoffModelModel->indexToType( currentIndex );

  ReosFormWidget *newForm = new ReosFormWidget( this );
  if ( runoffModel )
  {
    newForm->addParameters( runoffModel->parameters() );
  }
  else
  {
    if ( ReosRunoffModelRegistery::isInstantiate() )
      newForm->addText( ReosRunoffModelRegistery::instance()->modelDescription( runoffType ) );
  }

  if ( mCurrentForm )
  {
    ui->widgetEditor->layout()->replaceWidget( mCurrentForm, newForm );
    mCurrentForm->deleteLater();
    mCurrentForm = newForm;
  }
  else
  {
    mCurrentForm = newForm;
    ui->widgetEditor->layout()->addWidget( mCurrentForm );
  }
}

void ReosRunoffManager::onTreeViewContextMenu( const QPoint &pos )
{
  QMenu menu;
  QModelIndex index = ui->treeView->indexAt( pos );
  if ( !index.isValid() )
    return;

  QString runoffType = mRunoffModelModel->indexToType( index );
  ReosRunoffModel *runoffModel = mRunoffModelModel->runoffModel( index );

  if ( runoffType.isEmpty() )
    return;

  menu.addAction( tr( "Add a new model" ), this, [this, runoffType]
  {
    onAddNewModel( runoffType );
  } );

  if ( runoffModel )
  {
    menu.addAction( tr( "Remove this model" ), this, [this, runoffModel]
    {
      onRemoveRunoffModel( runoffModel );
    } );
  }

  menu.exec( ui->treeView->mapToGlobal( pos ) );
}

bool ReosRunoffManager::saveOn( const QString &fileName )
{
  if ( !ReosRunoffModelRegistery::isInstantiate() )
    return false;

  ReosRunoffModelRegistery *registery = ReosRunoffModelRegistery::instance();
  return registery->saveToFile( fileName, QStringLiteral( "runoff data" ) );
}

void ReosRunoffManager::selectRunoffModel( ReosRunoffModel *runoffModel )
{
  QModelIndex index = mRunoffModelModel->runoffModelToIndex( runoffModel );
  ui->treeView->expand( index );
  ui->treeView->setCurrentIndex( index );
}
