/***************************************************************************
  reosrenderersettings.h - ReosRendererSettings

 ---------------------
 begin                : 18.11.2022
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
#ifndef REOSRENDERERSETTINGS_H
#define REOSRENDERERSETTINGS_H

#include <QObject>

#include "reoscore.h"

class QDateTime;
class QLinearGradient;

class ReosRendererSettings
{
  public:
    virtual ~ReosRendererSettings();

    virtual QDateTime mapTime() const = 0;
};


class REOSCORE_EXPORT ReosColorShaderSettings : public QObject
{
    Q_OBJECT
  public:
    virtual ~ReosColorShaderSettings();

    virtual void getShader( void *shader ) const = 0;
    virtual void setShader( void *shader ) = 0;

    virtual bool isValid() const = 0;

    virtual double classificationMinimum() const = 0;
    virtual void setClassificationMinimum( double newClassificationMinimum ) = 0;

    virtual double classificationMaximum() const = 0;
    virtual void setClassificationMaximum( double newClassificationMaximum ) = 0;

    //! Returns ans set opacity, if opacity<0, opacity option is not available
    virtual double opacity() const = 0;
    virtual void setOpacity( double opacity ) = 0;

    virtual void getSourceMinMax( double &min, double &max ) const = 0;

    virtual void onSettingsUpdated() = 0 ;

    virtual QLinearGradient gradient() const = 0;

  signals:
    void settingsChangedFromObject();
    void changed();
};

#endif // REOSRENDERERSETTINGS_H
