/***************************************************************************
  reoshubeauhydrographprovider.h - ReosHubEauHydrographProvider

 ---------------------
 begin                : 6.11.2021
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
#ifndef REOSHUBEAUHYDROGRAPHPROVIDER_H
#define REOSHUBEAUHYDROGRAPHPROVIDER_H

#include "reostimeserieprovider.h"

class ReosHubEauConnectionControler;

class ReosHubEauHydrographProvider : public ReosTimeSerieVariableTimeStepProvider
{
    Q_OBJECT
  public:
    enum class Status
    {
      NoData,
      Loading,
      Loaded
    };

    ReosHubEauHydrographProvider() = default;

    QString key() const override;;
    QDateTime referenceTime() const override;
    void setReferenceTime( const QDateTime &referenceTime ) override;
    QString valueUnit() const override;
    int valueCount() const override;
    double value( int i ) const override;
    double firstValue() const override;
    double lastValue() const override;
    void load() override;
    double *data() override;
    const QVector<double> &constData() const override;
    ReosEncodedElement encode() const override;
    void decode( const ReosEncodedElement &element ) override;
    ReosDuration relativeTimeAt( int i ) const override;
    ReosDuration lastRelativeTime() const override;

    //! Returns the last status of  loading data
    Status status() const;

  private slots:
    void onResultReady( const QVariantMap &result );
    void onLoadingFinished();

  private:
    QDateTime mReferenceTime;
    ReosHubEauConnectionControler *mFlowRequestControler = nullptr;
    QVector<double> mCachedValues;
    QVector<ReosDuration> mCachedTimeValues;
    Status mStatus = Status::Loaded;
};

class ReosHubEauHydrographProviderFactory: public ReosDataProviderFactory
{
  public:
    ReosTimeSerieProvider *createProvider() const override {return new ReosHubEauHydrographProvider;};
    QString key() const override {return QStringLiteral( "hub-eau-hydrograph" );}
};




#endif // REOSHUBEAUHYDROGRAPHPROVIDER_H
