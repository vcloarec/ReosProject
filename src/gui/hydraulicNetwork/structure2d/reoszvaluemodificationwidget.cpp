/***************************************************************************
  reoszvaluemodificationwidget.cpp - ReosZValueModificationWidget

 ---------------------
 begin                : 11.3.2022
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
#include "reoszvaluemodificationwidget.h"
#include "ui_reoszvaluemodificationwidget.h"

#include "reosparameter.h"
#include "reossettings.h"

ReosZValueModificationWidget::ModificationType ReosZValueModificationWidget::modificationType() const
{
  ModificationType type =  static_cast<ModificationType>( ui->mTypeComboBox->currentData().toInt() );

  ReosSettings setting;
  setting.setValue( QStringLiteral( "ZValueModification/type" ), type );

  return type;
}

double ReosZValueModificationWidget::value()
{
  ReosSettings setting;
  setting.setValue( QStringLiteral( "ZValueModification/value" ), mValueParameter->value() );
  return mValueParameter->value();
}

ReosZValueModificationWidget::ReosZValueModificationWidget( QWidget *parent )
  : QDialog( parent )
  , ui( new Ui::ReosZValueModificationWidget )
  , mValueParameter( new ReosParameterDouble( tr( "Value" ), false, this ) )
{
  ui->setupUi( this );
  ui->mValueParamterWidget->setDouble( mValueParameter );

  ui->mTypeComboBox->addItem( tr( "New Value" ), ReosZValueModificationWidget::NewValue );
  ui->mTypeComboBox->addItem( tr( "Apply vertical offset" ), ReosZValueModificationWidget::Offset );

  ReosSettings setting;
  if ( setting.contains( "ZValueModification/value" ) )
    mValueParameter->setValue( setting.value( "ZValueModification/value" ).toDouble() );
  else
    mValueParameter->setValue( 0.0 );

  int type = 0;
  if ( setting.contains( "ZValueModification/type" ) )
    type = setting.value( "ZValueModification/type" ).toInt();

  ui->mTypeComboBox->setCurrentIndex( ui->mTypeComboBox->findData( type ) );
  ui->mValueParamterWidget->setFocusOnEdit();
}

ReosZValueModificationWidget::~ReosZValueModificationWidget()
{
  delete ui;
}
