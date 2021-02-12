/***************************************************************************
  reossyntheticrainfall.h - ReosSyntheticRainfall

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
#ifndef REOSSYNTHETICRAINFALL_H
#define REOSSYNTHETICRAINFALL_H

#include <QPointer>

#include "reostimeserie.h"
#include "reosidfcurves.h"

class ReosParameterDuration;

class ReosChicagoRainfall : public ReosTimeSerieConstantInterval
{
    Q_OBJECT
  public:
    ReosChicagoRainfall( QObject *parent );

    QString type() const override {return QStringLiteral( "chicago-rainfall" );}

    ReosParameterDuration *totalDuration();
    ReosParameterDouble *centerCoefficient();
    ReosIntensityDurationCurve *intensityDurationCurve() const;

    //! Sets the intensity duration curve and its uri, emit signal with uri if not empty, so becareful to not used it with uri from signal receiver
    void setIntensityDurationCurve( ReosIntensityDurationCurve *intensityDurationCurve, const QString &intensityDurationUri = QString() );

    //! Sets only the uri of the intensity duration curve, then the uri can be used to retrieve the source of the curve
    void setIntensityDurationUri( const QString &uri );

    //! Returns the uri of the intensity duration curve
    QString intensityDurationUri() const;

    ReosEncodedElement encode() const;
    //! Creates new instance from the encoded element
    static ReosChicagoRainfall *decode( const ReosEncodedElement &element, QObject *parent = nullptr );

  public slots:
    void updateRainfall();

  signals:
    void newIntensityDuration( const QString &intensityDurationUri );

  protected:
    ReosChicagoRainfall( const ReosEncodedElement &element, QObject *parent = nullptr );

  private:
    ReosParameterDuration *mTotalDuration;
    ReosParameterDouble *mCenterCoefficient;
    QPointer<ReosIntensityDurationCurve> mIntensityDurationCurve;

    //! This uri has to be used as a temporary information for location of the source.
    //! Could not be considered as persistent during runtime (for example, item can move)
    QString mIntensityDurationUri;

    void connectParameters();

};

#endif // REOSSYNTHETICRAINFALL_H
