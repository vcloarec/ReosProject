/***************************************************************************
                      reosdilaogbox.cpp
                     --------------------------------------
Date                 : 10-05-2019
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec at gmail dot com / projetreos at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reosdialogbox.h"

ReosDialogBox::ReosDialogBox( QWidget *w, QWidget *parent ): QDialog( parent ), mLayout( new QVBoxLayout() )
{
  setLayout( mLayout );
  QDialogButtonBox *buttonBox = new QDialogButtonBox( QDialogButtonBox::Ok | QDialogButtonBox::Cancel, this );
  if ( w )
    mLayout->addWidget( w );


  mLayout->addWidget( buttonBox );

  setSizePolicy( QSizePolicy::Maximum, QSizePolicy::Maximum );

  connect( buttonBox, SIGNAL( accepted() ), this, SLOT( accept() ) );
  connect( buttonBox, SIGNAL( rejected() ), this, SLOT( reject() ) );
}
