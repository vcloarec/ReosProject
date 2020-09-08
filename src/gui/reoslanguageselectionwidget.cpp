/***************************************************************************
                      reoslanguageselectionwidget.cpp
                     --------------------------------------
Date                 : 21-08-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reoslanguageselectionwidget.h"
#include "ui_reoslanguageselectionwidget.h"

ReosLanguageSelectionWidget::ReosLanguageSelectionWidget( QLocale locale, QWidget *parent ) :
  QDialog( parent ),
  ui( new Ui::ReosLanguageSelectionWidget )
{
  ui->setupUi( this );
  ui->comboBox->addItems( QStringList( {tr( "System language" ), tr( "French" ), tr( "English" )} ) );

  if ( locale == QLocale::system() )
    ui->comboBox->setCurrentIndex( 0 );

  if ( locale == QLocale::French )
    ui->comboBox->setCurrentIndex( 1 );

  if ( locale == QLocale::English )
    ui->comboBox->setCurrentIndex( 2 );
}

ReosLanguageSelectionWidget::~ReosLanguageSelectionWidget()
{
  delete ui;
}

QLocale ReosLanguageSelectionWidget::language()
{
  QLocale ret;

  int indexCombobox = ui->comboBox->currentIndex();

  switch ( indexCombobox )
  {
    case 0:
      ret = QLocale::system();
      break;
    case 1:
      ret = QLocale::French;
      break;
    case 2:
      ret = QLocale::English;
      break;
    default:
      ret = QLocale::system();
      break;
  }

  return ret;
}
