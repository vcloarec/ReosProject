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

#include "reosmodule.h"
#include "reosgisengine.h"

class ReosWatershedDelineating : public ReosModule
{
  public:
    //! State of the tool chain
    enum State
    {
      WaitingForDownstream,
      WaitingForExtent,
      WaitingWithBroughtBackExtent,
      WaitingforAutomaticDrawing,
      WaitingForValidate
    };

    ReosWatershedDelineating( ReosModule *parent, ReosGisEngine *gisEngine );
    bool hasValidDigitalElevationModel() const;
    bool setDigitalElevationModelDEM( const QString &layerId );

    State currentState() const;

    void setDownstreamLine( const QPolygonF &downstreamLine );

  private:
    ReosGisEngine *mGisEngine = nullptr;
    QString mDEMLayerId;
    State mCurrentState = WaitingForDownstream;
    QPolygonF mDownstreamLine;

};

#endif // REOSWATERSHEDDELINEATING_H
