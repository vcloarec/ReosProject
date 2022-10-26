#ifndef REOSHECRASPROJECT_H
#define REOSHECRASPROJECT_H

#include <QString>
#include <QMap>
#include <QPolygonF>
#include <memory>

class QTextStream;


class ReosHecRasGeometry
{
  public:
    ReosHecRasGeometry() = default;
    ReosHecRasGeometry( const QString fileName );

    const QString &title() {return mTitle;}

    int area2dCount() const;

  private:
    QString mFileName;
    QString mTitle;

    void parseGeometryFile();
    void parseStorageArea( QTextStream &stream, const QString storageName );

    QList<QPolygonF> m2dDomains;

};

class ReosHecRasProject
{
  public:
    ReosHecRasProject( const QString &projectFileName );

    int GeometriesCount() const;

    QStringList geometryIds() const;
    ReosHecRasGeometry geometry( const QString &id ) const;

  private:
    QString mFileName;
    QMap<QString, ReosHecRasGeometry> mGeometries;

    void parseProjectFile();

};

#endif // REOSHECRASPROJECT_H
