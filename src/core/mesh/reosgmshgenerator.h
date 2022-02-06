/***************************************************************************
  reosgmshgenerator.h - ReosGmeshGenerator

 ---------------------
 begin                : 14.1.2022
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
#ifndef REOSGMSHGENERATOR_H
#define REOSGMSHGENERATOR_H

#include <memory>
#include <functional>

#include "reosmeshgenerator.h"
#include "reosparameter.h"

class ReosGmshResolutionController: public ReosMeshResolutionController
{
  public:
    ReosGmshResolutionController( QObject *parent )
      : ReosMeshResolutionController( parent )
      , mDefaultSize( new ReosParameterDouble( tr( "Default element size" ), false, this ) )
    {
      mDefaultSize->setValue( 20 );
      connect( mDefaultSize, &ReosParameter::valueChanged, this, &ReosDataObject::dataChanged );
    };


    double sizeFallBack( int dim, int tag, double x, double y, double z, double lc )
    {
      return mDefaultSize->value();
    }

    ReosParameterDouble *defaultSize() const;

  private:
    ReosParameterDouble  *mDefaultSize;


};

class ReosGmshGenerator : public ReosMeshGenerator
{
  public:
    ReosGmshGenerator()
    {
    }

    ReosMeshFrameData generatedMesh( bool *ok ) const override;
    void setGeometryStructure( ReosPolylinesStructure *structure, const QString &crs ) override;
    void setResolutionController( ReosMeshResolutionController *resolutionControler ) override
    {
      mSizeControler = static_cast<ReosGmshResolutionController *>( resolutionControler );
    }

  private:
    ReosGmshResolutionController *mSizeControler;
};


#endif // REOSGMSHGENERATOR_H
