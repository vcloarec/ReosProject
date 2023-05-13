/***************************************************************************
  reoshydraulicschemewidget.cpp - ReosHydraulicSchemeWidget

 ---------------------
 begin                : 25.3.2022
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
#include "reoshydraulicschemewidget.h"
#include "ui_reoshydraulicschemewidget.h"

#include "reoshydraulicscheme.h"
#include "reoshydraulicnetwork.h"
#include "reoswatershedmodule.h"
#include "reosmeteorologicmodel.h"

ReosHydraulicSchemeWidget::ReosHydraulicSchemeWidget( const ReosHydraulicNetworkContext &context, QWidget *parent )
  : QWidget( parent )
  , ui( new Ui::ReosHydraulicSchemeWidget )
  , mMeteoCollection( context.watershedModule()->meteoModelsCollection() )
{
  ui->setupUi( this );
  ui->mMeteoModelCombo->setModel( mMeteoCollection );
  connect( ui->mMeteoModelCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosHydraulicSchemeWidget::onMeteoModelChange );
}

ReosHydraulicSchemeWidget::~ReosHydraulicSchemeWidget()
{
  delete ui;
}

void ReosHydraulicSchemeWidget::setScheme( ReosHydraulicScheme *scheme )
{
  mScheme = scheme;
  if ( scheme )
  {
    ui->mParameterNameWidget->setString( scheme->schemeName() );
    int meteoModelindex = mMeteoCollection->modelIndex( scheme->meteoModel() );

    if ( meteoModelindex != -1 )
      ui->mMeteoModelCombo->setCurrentIndex( meteoModelindex );
  }
}

void ReosHydraulicSchemeWidget::hideName()
{
  ui->mNameWidget->hide();
  ui->mNameSeprator->hide();
}

void ReosHydraulicSchemeWidget::onMeteoModelChange()
{
  if ( mScheme )
  {
    mScheme->setMeteoModel( mMeteoCollection->meteorologicModel( ui->mMeteoModelCombo->currentIndex() ) );
    QString meteoModelName;
    if ( mScheme->meteoModel() )
      meteoModelName = mScheme->meteoModel()->name()->value();
    emit meteoModelChange( meteoModelName );
  }
}

ReosHydraulicSchemeListView::ReosHydraulicSchemeListView( QWidget *parent ): QListView( parent )
{
}

void ReosHydraulicSchemeListView::setSchemeCollection( ReosHydraulicSchemeCollection *collection )
{
  mCollection = collection;
  setModel( collection );
}

ReosHydraulicScheme *ReosHydraulicSchemeListView::currentScheme() const
{
  return mCollection->scheme( currentIndex().row() );
}

void ReosHydraulicSchemeListView::setCurrentScheme( const QString &schemeId )
{
  setCurrentIndex( mCollection->index( mCollection->schemeIndex( schemeId ), 0, QModelIndex() ) );
}
