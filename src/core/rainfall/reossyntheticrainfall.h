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

#include "reosseriesrainfall.h"
#include "reosidfcurves.h"

class ReosParameterDuration;

class REOSCORE_EXPORT ReosUniqueIdfCurveSyntheticRainfall : public ReosSeriesRainfall
{
    Q_OBJECT
  public:
    ReosUniqueIdfCurveSyntheticRainfall( QObject *parent = nullptr );

    ReosParameterDuration *totalDuration() const;
    ReosParameterDouble *centerCoefficient() const;
    ReosIntensityDurationCurve *intensityDurationCurve() const;

    //! Sets the intensity duration curve and its uid, emit signal with uid if not empty, so becareful to not used it with uid from signal receiver (to prevent cycle connection)
    void setIntensityDurationCurve( ReosIntensityDurationCurve *intensityDurationCurve, const QString &intensityDurationUid = QString() );

    //! Sets only the unique id \a uid of the intensity duration curve, then the uid can be used to retrieve the source of the curve
    void setIntensityDurationUid( const QString &uid );

    //! Returns the unique id of the intensity duration curve
    QString intensityDurationUid() const;

    void encodeBase( ReosEncodedElement &element ) const;

  signals:
    void newIntensityDuration( const QString &intensityDurationUid );

  protected:
    void updateData() const override;

    ReosUniqueIdfCurveSyntheticRainfall( const ReosEncodedElement &element, QObject *parent = nullptr );
    void connectParameters();

    ReosParameterDuration *mTotalDuration = nullptr;
    ReosParameterDouble *mCenterCoefficient = nullptr;
    QPointer<ReosIntensityDurationCurve> mIntensityDurationCurve;

    QString mIntensityDurationUid;

  private:
    void init();
    virtual void updateRainfall() const = 0;

};

class REOSCORE_EXPORT ReosChicagoRainfall : public ReosUniqueIdfCurveSyntheticRainfall
{
    Q_OBJECT
  public:
    ReosChicagoRainfall( QObject *parent = nullptr );

    static QString staticType() {return ReosSeriesRainfall::staticType() + ':' + QStringLiteral( "chicago" );}
    QString type() const override {return staticType();}

    ReosEncodedElement encode() const;
    //! Creates new instance from the encoded element
    static ReosChicagoRainfall *decode( const ReosEncodedElement &element, QObject *parent = nullptr );

  protected:
    explicit ReosChicagoRainfall( const ReosEncodedElement &element, QObject *parent = nullptr );

  private:
    void updateRainfall() const override ;

};


class REOSCORE_EXPORT ReosAlternatingBlockRainfall : public ReosUniqueIdfCurveSyntheticRainfall
{
    Q_OBJECT
  public:
    ReosAlternatingBlockRainfall( QObject *parent = nullptr );

    static QString staticType() {return ReosSeriesRainfall::staticType() + ':' + QStringLiteral( "alternating-block" );}
    QString type() const override {return staticType();}

    ReosEncodedElement encode() const;
    //! Creates new instance from the encoded element
    static ReosAlternatingBlockRainfall *decode( const ReosEncodedElement &element, QObject *parent = nullptr );

  protected:
    explicit ReosAlternatingBlockRainfall( const ReosEncodedElement &element, QObject *parent = nullptr );

  private:
    void updateRainfall() const override ;
};

class REOSCORE_EXPORT ReosDoubleTriangleRainfall : public ReosSeriesRainfall
{
    Q_OBJECT
  public:
    ReosDoubleTriangleRainfall( QObject *parent = nullptr );

    static QString staticType() {return ReosSeriesRainfall::staticType() + ':' + QStringLiteral( "double-triangle" );}
    QString type() const override {return staticType();}

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

    //! Returns the uris of the intensity duration curve
    QString intensityDurationUniqueIdIntense() const;
    QString intensityDurationUniqueIdTotal() const;

    ReosEncodedElement encode() const;
    //! Creates new instance from the encoded element
    static ReosDoubleTriangleRainfall *decode( const ReosEncodedElement &element, QObject *parent = nullptr );

  signals:
    void newIntensityDuration( const QString &intensityDurationUniqueIdIntense, const QString &intensityDurationUniqueIdTotal );

  protected:
    explicit ReosDoubleTriangleRainfall( const ReosEncodedElement &element, QObject *parent = nullptr );
    void updateData() const override;

  private:
    ReosParameterDuration *mIntenseDuration;
    ReosParameterDuration *mTotalDuration;
    ReosParameterDouble *mCenterCoefficient;
    QPointer<ReosIntensityDurationCurve> mIntensityDurationCurveIntense;
    QPointer<ReosIntensityDurationCurve> mIntensityDurationCurveTotal;

    QString mIntensityDurationUniqueIdIntense;
    QString mIntensityDurationUniqueIdTotal;

    void connectParameters();
    void updateRainfall() const;
};



#endif // REOSSYNTHETICRAINFALL_H
