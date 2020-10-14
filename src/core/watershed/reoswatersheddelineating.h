/***************************************************************************
                      reoswatersheddelineating.h
                     --------------------------------------
Date                 : 04-10-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOSWATERSHEDDELINEATING_H
#define REOSWATERSHEDDELINEATING_H

#include <memory>

#include "reosgisengine.h"
#include "reosmapextent.h"
#include "reosmodule.h"
#include "reosprocess.h"
#include "reoswatershed.h"
#include "reosrasterwatershed.h"

class ReosRasterFillingWangLiu;

class ReosWatershedDelineatingProcess: public ReosProcess
{

  public:
    ReosWatershedDelineatingProcess( ReosDigitalElevationModel *dem, const ReosMapExtent &mapExtent,  const QPolygonF &downtreamLine );

    void start();

  private:
    ReosMapExtent mExtent;
    std::unique_ptr<ReosDigitalElevationModel> mEntryDem;
    QPolygonF mDownstreamLine;
};


class ReosWatershedDelineating : public ReosModule
{
  public:
    //! State of the tool chain
    enum State
    {
      NoDigitalElevationModel,
      WaitingForDownstream,
      WaitingForExtent,
      WaitingWithBroughtBackExtent,
      WaitingforProceed,
      WaitingForValidate
    };

    ReosWatershedDelineating( ReosModule *parent, ReosGisEngine *gisEngine );
    State currentState() const;


    bool hasValidDigitalElevationModel() const;
    bool setDigitalElevationModelDEM( const QString &layerId );

    bool setDownstreamLine( const QPolygonF &downstreamLine );
    bool setPreDefinedExtent( const ReosMapExtent &extent );

    ReosProcess *delineatingProcess();

  private:
    ReosGisEngine *mGisEngine = nullptr;
    QString mDEMLayerId;
    State mCurrentState = NoDigitalElevationModel;
    QPolygonF mDownstreamLine;
    ReosMapExtent mExtent;
};

#endif // REOSWATERSHEDDELINEATING_H
