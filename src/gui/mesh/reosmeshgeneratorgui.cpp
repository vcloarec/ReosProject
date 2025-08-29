/***************************************************************************
  reosmeshgeneratorgui.cpp - ReosMeshGeneratorGui

 ---------------------
 begin                : 13.2.2022
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
#include "reosmeshgeneratorgui.h"

#include <QHBoxLayout>
#include <QLabel>
#include "gmsh/reosgmshgenerator.h"

ReosFormWidget *ReosFormGmshGeneratorWidgetFactory::createDataWidget( ReosDataObject *dataObject, const ReosGuiContext &context )
{
  ReosGmshGenerator *generator = qobject_cast<ReosGmshGenerator *>( dataObject );

  if ( !generator )
    return nullptr;

  ReosFormWidget *w = new ReosFormWidget( context.parent() );

  QWidget *algWidget = new QWidget( w );
  algWidget->setLayout( new QHBoxLayout );

  QComboBox *combo = new QComboBox( algWidget );
  algWidget->layout()->addWidget( new QLabel( QObject::tr( "Gmsh algoritm" ), algWidget ) );
  algWidget->layout()->addWidget( combo );

  for ( int i = 0; i < ReosGmshGenerator::AlgCount; ++i )
  {
    ReosGmshGenerator::Algorithm alg = static_cast<ReosGmshGenerator::Algorithm>( i );
    combo->addItem( ReosGmshGenerator::algorithmName( alg ), alg );
  }

  combo->setCurrentIndex( combo->findData( generator->algorithm() ) );
  w->addWidget( algWidget );

  QObject::connect( combo, QOverload<int>::of( &QComboBox::currentIndexChanged ), generator, [generator, combo]
  {
    generator->setAlgorithm( static_cast<ReosGmshGenerator::Algorithm>( combo->currentData().toInt() ) );
  } );

  return w;
}

QString ReosFormGmshGeneratorWidgetFactory::datatype() const
{
  return ReosGmshGenerator::staticType();
}
