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
#include "reosmodule.h"

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

    QString key() const override;
    QStringList fileSuffixes() const override;
    QDateTime referenceTime() const override;
    QString valueUnit() const override;
    int valueCount() const override;
    double value( int i ) const override;
    double firstValue() const override;
    double lastValue() const override;
    void load() override;
    double *data() override;
    const QVector<ReosDuration> &constTimeData() const override;
    const QVector<double> &constData() const override;
    ReosEncodedElement encode( const ReosEncodeContext &context ) const override;
    void decode( const ReosEncodedElement &element, const ReosEncodeContext & ) override;
    ReosDuration relativeTimeAt( int i ) const override;
    ReosDuration lastRelativeTime() const override;
    QString htmlDescription() const override;
    bool isLoading() const override;

    //! Returns the last status of  loading data
    Status status() const;

    //! Returns the meta data linked to this provider
    QVariantMap metadata() const;

    //! Sets the meta data of this provider
    void setMetadata( const QVariantMap &metadata );

    //! Returns the last message produced during loading data
    ReosModule::Message lastMessage() const;

    static QString htmlDescriptionFromMeta( const QVariantMap &metadata );

    //! Returns the key of this provider
    static QString staticKey();

    static QString dataType();

  signals:
    void errorOccured();

  private slots:
    void onResultReady( const QVariantMap &result );
    void onLoadingFinished();
    void onMetadataReady( const QVariantMap &result );
    void onErrorOccured();

  private:
    QDateTime mReferenceTime;
    ReosHubEauConnectionControler *mFlowRequestControler = nullptr;
    ReosHubEauConnectionControler *mMetadataRequestControler = nullptr;
    QVariantMap mMetadata;
    QVector<double> mCachedValues;
    QVector<ReosDuration> mCachedTimeValues;
    Status mStatus = Status::Loaded;
    ReosModule::Message mLastMessage;

};

class ReosHubEauHydrographProviderFactory: public ReosDataProviderFactory
{
  public:
    ReosTimeSerieProvider *createProvider( const QString &dataType ) const override;;
    QString key() const override;
    bool hasCapabilities( const QString &dataType, ReosDataProvider::Capabilities capabilities ) const override;

  private:
    ReosDataProvider::Capabilities mCapabilities = {ReosDataProvider::Spatial};
};




#endif // REOSHUBEAUHYDROGRAPHPROVIDER_H
