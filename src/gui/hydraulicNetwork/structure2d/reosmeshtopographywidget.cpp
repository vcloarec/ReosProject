/***************************************************************************
  reosmeshtopographywidget.cpp - ReosMeshTopographyWidget

 ---------------------
 begin                : 25.2.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosmeshtopographywidget.h"
#include "ui_reosmeshtopographywidget.h"

#include <QHBoxLayout>
#include <QMenu>
#include <QMessageBox>

#include "reosmeshscalarrenderingwidget.h"
#include "reosmap.h"
#include "reosdigitalelevationmodel.h"
#include "reostopographycollection.h"
#include "reosguicontext.h"
#include "reosstyleregistery.h"
#include "reosprocesscontroler.h"

ReosMeshTopographyStackedWidget::ReosMeshTopographyStackedWidget(
  ReosMesh *mesh,
  ReosTopographyCollection *topographyCollection,
  const QString &topographyDatasetId,
  const ReosGuiContext &guiContext )
  : ReosActionStackedWidget( guiContext.parent() )
{
  layout()->setContentsMargins( 0, 0, 0, 0 );
  addPage( new ReosMeshTopographyWidget( mesh, topographyCollection, topographyDatasetId, ReosGuiContext( guiContext, this ) ) );
}

ReosMeshTopographyWidget::ReosMeshTopographyWidget( ReosMesh *mesh, ReosTopographyCollection *topographyCollection, const QString &topographyDatasetId, const ReosGuiContext &guiContext )
  : ReosStackedPageWidget( guiContext.parent() )
  , ui( new Ui::ReosMeshTopographyWidget )
  , mMesh( mesh )
  , mTopographyDatasetId( ( topographyDatasetId ) )
  , mGuiContext( guiContext )
  , mTopographyCollection( topographyCollection )
  , mCollectionModel( new ReosTopographyCollectionListModel( topographyCollection, this ) )
{
  ui->setupUi( this );
  ui->mDemCombo->setGisEngine( guiContext.map()->engine() );
  ui->mTopographyCollectionView->setModel( mCollectionModel );
  ui->mTopographyCollectionView->setContextMenuPolicy( Qt::ContextMenuPolicy::CustomContextMenu );
  connect( ui->mTopographyCollectionView, &QWidget::customContextMenuRequested, this, &ReosMeshTopographyWidget::onViewContextMenuRequest );
  ui->mRenderingSettingsButton->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize( this ) );
  ui->mAutoApplyWidget->setBooleanParameter( topographyCollection->autoApply() );

  connect( ui->mAddTopographyToolButton, &QToolButton::clicked, this, &ReosMeshTopographyWidget::onAddTopography );
  connect( ui->mApplyTopographyButton, &QToolButton::clicked, this, &ReosMeshTopographyWidget::applyDem );
  connect( ui->mRenderingSettingsButton, &QToolButton::clicked, this, &ReosMeshTopographyWidget::onRenderingSettings );
  connect( mGuiContext.map(), &ReosMap::cursorMoved, this, &ReosMeshTopographyWidget::onMapCursorMove );
}

void ReosMeshTopographyWidget::applyDem()
{
  std::unique_ptr<ReosProcess> process( mMesh->applyTopographyOnVertices( mTopographyCollection ) );
  ReosProcessControler *controler = new ReosProcessControler( process.get(), this );

  controler->exec();

  controler->deleteLater();
}

void ReosMeshTopographyWidget::onAddTopography()
{
  QString topoId = ui->mDemCombo->currentDemLayerId();

  if ( mTopographyCollection->contains( topoId ) )
  {
    QMessageBox::information( this, tr( "Add a Topography to the Collection" ),
                              tr( "The topography collection already contains this topography." ) );
    return;
  }
  mTopographyCollection->insertTopography( 0, topoId );
}

void ReosMeshTopographyWidget::onRenderingSettings()
{
  emit addOtherPage( new ReosMeshScalarRenderingWidget( mMesh->terrainColorShaderSettings(), ReosGuiContext( mGuiContext, this ) ) );
}

void ReosMeshTopographyWidget::onMapCursorMove( const QPointF &pos )
{
  double topographyValue = mMesh->datasetScalarValueAt( mTopographyDatasetId, pos );

  if ( std::isnan( topographyValue ) )
    ui->mMeshZValueLabel->setText( tr( "No value" ) );
  else
    ui->mMeshZValueLabel->setText( QLocale().toString( topographyValue, 'f', 2 ) );
}

void ReosMeshTopographyWidget::onViewContextMenuRequest( const QPoint &pos )
{
  QModelIndex index = ui->mTopographyCollectionView->currentIndex();
  if ( !index.isValid() )
    return;

  QMenu *menu = new QMenu( this );
  int row = index.row();
  QAction *action = menu->addAction( tr( "Remove selected topography" ), menu, [this, row]
  {
    mTopographyCollection->removeTopography( row );
  } );

  action->setIcon( QIcon( QStringLiteral( ":/images/remove.svg" ) ) );

  menu->exec( ui->mTopographyCollectionView->viewport()->mapToGlobal( pos ) );

}
