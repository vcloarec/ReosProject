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

class ReosSerieRainfall : public ReosTimeSerieConstantInterval
{
    Q_OBJECT
  public:
    ReosSerieRainfall( QObject *parent = nullptr );
    QString type() const override {return QStringLiteral( "serie-rainfall" );}

    ReosEncodedElement encode() const;

    //! Creates new instance from the encoded element
    static ReosSerieRainfall *decode( const ReosEncodedElement &element, QObject *parent = nullptr );

  protected:
    ReosSerieRainfall( const ReosEncodedElement &element, QObject *parent = nullptr );

  private:
    void setUpdata();

};

class REOSCORE_EXPORT ReosChicagoRainfall : public ReosSerieRainfall
{
    Q_OBJECT
  public:
    ReosChicagoRainfall( QObject *parent = nullptr );

    QString type() const override {return QStringLiteral( "chicago-rainfall" );}

    ReosParameterDuration *totalDuration();
    ReosParameterDouble *centerCoefficient();
    ReosIntensityDurationCurve *intensityDurationCurve() const;

    //! Sets the intensity duration curve and its uid, emit signal with uid if not empty, so becareful to not used it with uid from signal receiver (to prevent cycle connection)
    void setIntensityDurationCurve( ReosIntensityDurationCurve *intensityDurationCurve, const QString &intensityDurationUid = QString() );

    //! Sets only the unique id \a uid of the intensity duration curve, then the uid can be used to retrieve the source of the curve
    void setIntensityDurationUid( const QString &uid );

    //! Returns the unique id of the intensity duration curve
    QString intensityDurationUid() const;

    ReosEncodedElement encode() const;
    //! Creates new instance from the encoded element
    static ReosChicagoRainfall *decode( const ReosEncodedElement &element, QObject *parent = nullptr );

  public slots:
    void updateRainfall();

  signals:
    void newIntensityDuration( const QString &intensityDurationUid );

  protected:
    ReosChicagoRainfall( const ReosEncodedElement &element, QObject *parent = nullptr );

  private:
    ReosParameterDuration *mTotalDuration;
    ReosParameterDouble *mCenterCoefficient;
    QPointer<ReosIntensityDurationCurve> mIntensityDurationCurve;

    QString mIntensityDurationUid;

    void connectParameters();
};

class REOSCORE_EXPORT ReosDoubleTriangleRainfall : public ReosSerieRainfall
{
    Q_OBJECT
  public:
    ReosDoubleTriangleRainfall( QObject *parent );

    QString type() const override {return QStringLiteral( "double-triangle-rainfall" );}

    ReosParameterDuration *totalDuration();
    ReosParameterDuration *intenseDuration();
    ReosParameterDouble *centerCoefficient();
    ReosIntensityDurationCurve *intensityDurationCurveIntensePeriod() const;
    ReosIntensityDurationCurve *intensityDurationCurveTotal() const;

    //! Sets the intensity duration curves and their unique id, emit signal with uids if not empty, so be careful to not used it with uid from signal receiver
    void setIntensityDurationCurve( ReosIntensityDurationCurve *intensityDurationCurveIntense,
                                    ReosIntensityDurationCurve *intensityDurationCurveTotal,
                                    const QString &intensityDurationUniqueIdIntense = QString(),
                                    const QString &intensityDurationUniqueIdTotal = QString() );

    //! Sets only the uids of the intensity duration curves, then the uid can be used to retrieve the source of the curve
    void setIntensityDurationUniqueId( const QString &intenseUid, const QString &totalUid );

    //! Returns the uis of the intensity duration curve
    QString intensityDurationUniqueIdIntense() const;
    QString intensityDurationUniqueIdTotal() const;

    ReosEncodedElement encode() const;
    //! Creates new instance from the encoded element
    static ReosDoubleTriangleRainfall *decode( const ReosEncodedElement &element, QObject *parent = nullptr );

  public slots:
    void updateRainfall();

  signals:
    void newIntensityDuration( const QString &intensityDurationUniqueIdIntense, const QString &intensityDurationUniqueIdTotal );

  protected:
    ReosDoubleTriangleRainfall( const ReosEncodedElement &element, QObject *parent = nullptr );

  private:
    ReosParameterDuration *mIntenseDuration;
    ReosParameterDuration *mTotalDuration;
    ReosParameterDouble *mCenterCoefficient;
    QPointer<ReosIntensityDurationCurve> mIntensityDurationCurveIntense;
    QPointer<ReosIntensityDurationCurve> mIntensityDurationCurveTotal;

    QString mIntensityDurationUniqueIdIntense;
    QString mIntensityDurationUniqueIdTotal;

    void connectParameters();
};



#endif // REOSSYNTHETICRAINFALL_H
