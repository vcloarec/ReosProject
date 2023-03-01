/***************************************************************************
  reoshecrascontroller.h - ReosHecRasController

 ---------------------
 begin                : 03.10.2022
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
#ifndef REOSHECRASCONTROLLER_H
#define REOSHECRASCONTROLLER_H

#ifdef _WIN32
#include <combaseapi.h>
#undef min
#undef max
#endif

#include <QPolygon>
#include <QMap>
#include <QObject>


class ReosHecRasController : public QObject
{
    Q_OBJECT
  public:
    //! Constructor with \a version of HecRas, \see availableVersion()
    explicit ReosHecRasController( const QString &version );
    ~ReosHecRasController();

    //! Returns available version of HecRas controller
    static QStringList availableVersion();

    //! Returns whether the controller is valid
    bool isValid() const;

    //! Sets the current plan
    void setCurrentPlan( const QString &currentPlan );

    //! Sets the current project file name
    void setProjectFileName( const QString &newProjectFileName );

    bool isSuccessful() const;

public slots:
    void startComputation();

  signals:
    void sendInformation( const QString &info );

  private:

    QString mVersion;
    QString mProjectFileName;
    QString mCurrentPlan;
    bool mIsValid = false;
    bool mIsSuccessful = false;
#ifdef _WIN32
    IDispatch *mDispatch = nullptr;
    QMap<QString, DISPID> mFunctionNames;
#endif

    bool exitRas() const;

    bool setCurrentPlanPrivate( const QString &planName );

    //! Initializes the controller before using it. ONce initialized, the controller must be used in the thread where is has been initializd.
    void initialize();

    //! Returns the domain of the flow area with \a areaName
    QPolygonF flow2DAreasDomain( const QString &areaName ) const;

    //! Returns the flow 2D area names of the currently opened project
    QStringList flowAreas2D() const;

    //! Returns the plan names of the currently opened project
    QStringList planNames() const;

    //! Returns the user-friendly string of the controller version of this instance
    QString version() const;

    //! Opens a project with path \a projFileName
    bool openHecrasProject( const QString &projFileName );

    //! Starts computation of the current plan
    QStringList computeCurrentPlan();

    bool showRas() const;

    bool showComputationWindow() const;

    bool hideComputationWindow() const;

    friend class ReosHecrasTesting;

};


#endif // REOSHECRASCONTROLLER_H
