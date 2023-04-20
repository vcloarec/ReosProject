/***************************************************************************
  reosdelftfewsxmlprovider.h - ReosDelftFewsXMLProvider

 ---------------------
 begin                : 10.11.2021
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
#ifndef REOSDELFTFEWSXMLPROVIDER_H
#define REOSDELFTFEWSXMLPROVIDER_H

#include "reostimeseriesprovider.h"

class QDomElement;
class QDomDocument;

class ReosDelftFewsXMLProviderInterface
{
  public:

    QVariantMap metadata() const;
    void setMetadata( const QVariantMap &meta );

    static QString staticKey() {return QStringLiteral( "delft-fews-xml" );}
    static QDateTime timefromElement( const QDomElement &element );
    static double doubleValueFromElement( const QDomElement &element, const QString &noData );
    static QString valueStringFromElement( const QDomElement &element );
    static QString htmlDescriptionFromMetada( const QVariantMap &metadata );

    static QString buildUri( const QString &fileName, const QString &stationId, const QDateTime &startTime, const QDateTime &endTime );

  protected:
    ReosDelftFewsXMLProviderInterface();
    static QString fileNameFromUri( const QString &uri );
    static QString stationIdFromUri( const QString &uri );
    static QDateTime endTimeFromUri( const QString &uri );
    static QDateTime startTimeFromUri( const QString &uri );

    static QString changeFileNameInUri( const QString &uri, const QString &fileName );

    QDomElement seriesElement( const QString &uri, QDomDocument &document ) const;

  private:
    QVariantMap mMeta;
};

class ReosDelftFewsXMLRainfallProvider: public ReosTimeSerieConstantTimeStepProvider, public ReosDelftFewsXMLProviderInterface
{
    Q_OBJECT
  public:

    QString key() const override;
    QStringList fileSuffixes() const override;
    QString htmlDescription() const override;
    void load() override;
    // ReosTimeSerieProvider interface
    QDateTime referenceTime() const override;
    QString valueUnit() const override;
    int valueCount() const override;
    double value( int i ) const override;
    double firstValue() const override;
    double lastValue() const override;
    double *data() override;
    const QVector<double> &constData() const override;
    ReosEncodedElement encode( const ReosEncodeContext &context ) const override;
    void decode( const ReosEncodedElement &element, const ReosEncodeContext &context ) override;

    // ReosTimeSerieConstantTimeStepProvider interface
    ReosDuration timeStep() const override;

    static QString dataType();
    static QString oldDataType();

  private:
    QDateTime mReferenceTime;
    ReosDuration mTimeStep;
    QVector<double> mCacheValues;
};

class ReosDelftFewsXMLHydrographProvider: public ReosTimeSerieVariableTimeStepProvider, public ReosDelftFewsXMLProviderInterface
{
    Q_OBJECT
  public:

    QString key() const override;
    QStringList fileSuffixes() const override;
    QString htmlDescription() const override;
    void load() override;
    QDateTime referenceTime() const override;
    QString valueUnit() const override;
    int valueCount() const override;
    double value( int i ) const override;
    double firstValue() const override;
    double lastValue() const override;
    double *data() override;
    const QVector<double> &constData() const override;
    const QVector<ReosDuration> &constTimeData() const override;
    ReosEncodedElement encode( const ReosEncodeContext &context ) const override;
    void decode( const ReosEncodedElement &element, const ReosEncodeContext &context ) override;

    // ReosTimeSerieVariableTimeStepProvider interface
    ReosDuration relativeTimeAt( int i ) const override;
    ReosDuration lastRelativeTime() const override;

    static QString dataType();

  private:
    QDateTime mReferenceTime;
    QVector<ReosDuration> mCacheTimeValues;
    QVector<double> mCacheValues;
};

class ReosDelftFewsXMLProviderFactory: public ReosDataProviderFactory
{
  public:
    ReosTimeSerieProvider *createProvider( const QString &dataType ) const override;
    QString key() const override;
    bool hasCapabilities( const QString &dataType, ReosDataProvider::Capabilities capabilities ) const override;

  private:
    ReosDataProvider::Capabilities mCapabilities = {ReosDataProvider::Spatial};
};

#endif // REOSDELFTFEWSXMLPROVIDER_H
