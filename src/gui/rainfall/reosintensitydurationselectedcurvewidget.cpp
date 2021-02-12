/***************************************************************************
  reosintensitydurationselectedcurvewidget.cpp - ReosIntensityDurationSelectedCurveWidget

 ---------------------
 begin                : 10.2.2021
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
#include "reosintensitydurationselectedcurvewidget.h"
#include "ui_reosintensitydurationselectedcurvewidget.h"

#include <QDialogButtonBox>
#include <QPushButton>
#include <QToolButton>
#include <QTreeView>
#include <QHeaderView>

#include "reosrainfallmodel.h"
#include "reosrainfallregistery.h"

ReosRainfallItemSelectionDialog::ReosRainfallItemSelectionDialog( QWidget *parent ):
  QDialog( parent )
  , mTreeView( new QTreeView( this ) )
  , mTextLabel( new QLabel( this ) )
{
  setLayout( new QVBoxLayout );
  layout()->addWidget( mTextLabel );
  layout()->addWidget( mTreeView );

  if ( ReosRainfallRegistery::isInstantiate() )
  {
    mModel = ReosRainfallRegistery::instance()->rainfallModel();
    mTreeView->setModel( mModel );
  }

  mTreeView->header()->setSectionResizeMode( QHeaderView::ResizeToContents );
  mTreeView->expandAll();

  mButtonBox = new QDialogButtonBox( QDialogButtonBox::Ok | QDialogButtonBox::Cancel, this );
  layout()->addWidget( mButtonBox );
  connect( mTreeView->selectionModel(), &QItemSelectionModel::selectionChanged, this, &ReosRainfallItemSelectionDialog::onSelectionChange );
  connect( mButtonBox, &QDialogButtonBox::rejected, this, &QDialog::reject );
  connect( mButtonBox, &QDialogButtonBox::accepted, this, &QDialog::accept );
  onSelectionChange();
}

void ReosRainfallItemSelectionDialog::setSelectionType( ReosRainfallItem::Type type, QString dataType )
{
  mSelectionType = type;
  mSelectionDataType = dataType;
  onSelectionChange();
}

void ReosRainfallItemSelectionDialog::setText( const QString &text )
{
  mTextLabel->setText( text );
}

ReosRainfallItem *ReosRainfallItemSelectionDialog::selectedItem() const
{
  return mModel->indexToItem( mTreeView->currentIndex() );
}

void ReosRainfallItemSelectionDialog::onSelectionChange()
{
  if ( !mModel )
    return;
  QModelIndex currentIndex = mTreeView->currentIndex();

  ReosRainfallItem *item = mModel->indexToItem( currentIndex );
  bool selectionIsGood = ( item && item->type() == mSelectionType );
  if ( item && item->type() == ReosRainfallItem::Data )
  {
    ReosRainfallDataItem *dataItem = qobject_cast<ReosRainfallDataItem *>( item );
    selectionIsGood &= ( dataItem != nullptr && dataItem->dataType() == mSelectionDataType );
  }

  mButtonBox->button( QDialogButtonBox::Ok )->setEnabled( selectionIsGood );
}

ReosIntensityDurationSelectedCurveWidget::ReosIntensityDurationSelectedCurveWidget( QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosIntensityDurationSelectedCurveWidget )
{
  ui->setupUi( this );

  connect( ui->toolButtonCurve, &QToolButton::clicked, this, &ReosIntensityDurationSelectedCurveWidget::onToolButtonCurve );
}

ReosIntensityDurationSelectedCurveWidget::~ReosIntensityDurationSelectedCurveWidget()
{
  delete ui;
}

ReosRainfallIntensityDurationCurveItem *ReosIntensityDurationSelectedCurveWidget::curveItem() const
{
  return mCurveItem;
}

void ReosIntensityDurationSelectedCurveWidget::onToolButtonCurve()
{
  ReosRainfallItemSelectionDialog *dialog = new ReosRainfallItemSelectionDialog( this );
  dialog->setText( tr( "Choose a Intensity Duration Curve:" ) );
  dialog->setSelectionType( ReosRainfallItem::Data, QStringLiteral( "id-curve" ) );

  if ( dialog->exec() )
  {
    ReosRainfallIntensityDurationCurveItem *curveItem = qobject_cast<ReosRainfallIntensityDurationCurveItem *>( dialog->selectedItem() );
    if ( !curveItem )
      return;

    if ( mCurveItem )
      disconnect( mCurveItem, &ReosRainfallIntensityDurationCurveItem::changed, this, &ReosIntensityDurationSelectedCurveWidget::onCurveChanged );

    mCurveItem = curveItem;
    if ( mCurveItem )
      connect( mCurveItem, &ReosRainfallIntensityDurationCurveItem::changed, this, &ReosIntensityDurationSelectedCurveWidget::onCurveChanged );

    onCurveChanged();
  }
}

void ReosIntensityDurationSelectedCurveWidget::onCurveChanged()
{
  ui->labelReturnPeriod->setText( QString() );
  ui->labelStation->setText( QString() );
  ui->labelName->setText( QString() );

  if ( mCurveItem )
  {
    if ( mCurveItem->data() )
      ui->labelReturnPeriod->setText( mCurveItem->data()->returnPeriod()->toString( 0 ) );

    if ( mCurveItem->parentItem() )
    {
      ui->labelName->setText( mCurveItem->parentItem()->name() );
      if ( mCurveItem->parentItem()->parentItem() )
        ui->labelStation->setText( mCurveItem->parentItem()->parentItem()->name() );
    }
  }

  emit curveChanged( mCurveItem );
}

void ReosIntensityDurationSelectedCurveWidget::setCurveItem( ReosRainfallIntensityDurationCurveItem *curveItem )
{
  mCurveItem = curveItem;
  onCurveChanged();
}
