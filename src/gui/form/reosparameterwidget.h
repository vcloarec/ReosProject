/***************************************************************************
  reosparameterwidget.h - ReosParameterWidget

 ---------------------
 begin                : 22.1.2021
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
#ifndef REOSPARAMETERWIDGET_H
#define REOSPARAMETERWIDGET_H

#include <QPointer>
#include <QWidget>
#include <QComboBox>
#include <QTextEdit>
#include <QWidgetAction>

class QLabel;
class QLineEdit;
class QComboBox;
class QToolButton;
class QDateTimeEdit;
class QBoxLayout;
class QSpacerItem;
class QCheckBox;

#include "reosgui.h"
#include "reosparameter.h"

class REOSGUI_EXPORT ReosParameterWidget : public QWidget
{
    Q_OBJECT
  public:
    enum SpacerPosition
    {
      NoSpacer = 1 << 0,
      SpacerBefore = 1 << 1, //!< Include a spacer before the label
      SpacerInMiddle = 1 << 2, //!< Include a spacer between label and value
      SpacerAfter = 1 << 3, //!< Include a spacer after the value
    };

    explicit ReosParameterWidget( const QString &defaultName, QWidget *parent = nullptr, Qt::Orientation orientation = Qt::Horizontal );

    //! Sets the keyboard focus on the edit part of the widget
    virtual void setFocusOnEdit() = 0;
    static ReosParameterWidget *createWidget( ReosParameter *parameter, QWidget *parent = nullptr );

    void enableSpacer( SpacerPosition spacerPosition );
    void setDefaultName( const QString &defaultName );
    void hideWhenVoid( bool b );
    void canBeAlwaysHidden();
    void hideLabel();

  public slots:
    //! Updates the dispayed value from the parameter
    virtual void updateValue() = 0;
    //! Sets the parameter value with the displayed value
    virtual void applyValue() = 0;
    virtual void askDerivation();

  signals:
    void valueChanged();
    void unitChanged();

  protected:
    void finalizeWidget();
    void setParameter( ReosParameter *param );
    bool mHideWhenVoid = true;
    bool mCanBeAlwaysHidden = true;
    QPointer<ReosParameter> mParameter = nullptr;

  private:
    QLabel *mLabelName = nullptr;
    QToolButton *mDerivationButton = nullptr;
    QBoxLayout *mLayout = nullptr;
    QSpacerItem *mSpacerBefore = nullptr;
    QSpacerItem *mSpacerMiddle = nullptr;
    QSpacerItem *mSpacerAfter = nullptr;
    QString mDefaultName;
};

class REOSGUI_EXPORT ReosParameterWidgetAction : public QWidgetAction
{
  public:
    ReosParameterWidgetAction( ReosParameter *parameter, QObject *parent = nullptr );

    QWidget *createWidget( QWidget *parent ) override;

  private:
    QPointer<ReosParameter> mParameter;
};


class REOSGUI_EXPORT ReosParameterInLineWidget : public ReosParameterWidget
{
  public:
    ReosParameterInLineWidget( QWidget *parent = nullptr, const QString &defaultName = QString() );
    void setFocusOnEdit() override;

  protected:
    void setTextValue( double value );
    void setTextValue( const QString &str );
    double value() const;
    QString textValue() const;
    bool textHasChanged() const;
  private:
    QLineEdit *mLineEdit = nullptr;
    mutable QString mCurrentText;
};

class REOSGUI_EXPORT ReosParameterDoubleWidget : public ReosParameterInLineWidget
{
    Q_OBJECT
  public:
    explicit ReosParameterDoubleWidget( QWidget *parent = nullptr, const QString &defaultName = QString() );
    explicit ReosParameterDoubleWidget( ReosParameterDouble *value, QWidget *parent = nullptr );

    void setDouble( ReosParameterDouble *value );
    void updateValue();
    void applyValue();
    static QString type() {return QStringLiteral( "double" );}
    ReosParameterDouble *doubleParameter();
};

class REOSGUI_EXPORT ReosParameterIntegerWidget : public ReosParameterInLineWidget
{
    Q_OBJECT
  public:
    explicit ReosParameterIntegerWidget( QWidget *parent = nullptr, const QString &defaultName = QString() );
    explicit ReosParameterIntegerWidget( ReosParameterInteger *value, QWidget *parent = nullptr );

    void setInteger( ReosParameterInteger *value );
    void updateValue();
    void applyValue();
    static QString type() {return QStringLiteral( "integer" );}
    ReosParameterInteger *integerParameter();
};

class REOSGUI_EXPORT ReosParameterStringWidget : public ReosParameterInLineWidget
{
    Q_OBJECT
  public:
    explicit ReosParameterStringWidget( QWidget *parent = nullptr, const QString &defaultName = QString() );
    explicit ReosParameterStringWidget( ReosParameterString *string, QWidget *parent = nullptr );

    void setString( ReosParameterString *string );
    void updateValue();
    void applyValue();

    static QString type() {return QStringLiteral( "string" );}
    ReosParameterString *stringParameter();
};

class REOSGUI_EXPORT ReosParameterAreaWidget: public ReosParameterInLineWidget
{
    Q_OBJECT
  public:
    explicit ReosParameterAreaWidget( QWidget *parent = nullptr, const QString &defaultName = QString() );
    explicit ReosParameterAreaWidget( ReosParameterArea *area, QWidget *parent = nullptr );

    void setArea( ReosParameterArea *area );
    void updateValue();
    void applyValue();

    static QString type() {return QStringLiteral( "area" );}

    ReosParameterArea *areaParameter() const;

  private:
    QComboBox *mUnitCombobox = nullptr;
};

class REOSGUI_EXPORT ReosParameterSlopeWidget: public ReosParameterInLineWidget
{
    Q_OBJECT
  public:
    explicit ReosParameterSlopeWidget( QWidget *parent = nullptr, const QString &defaultName = QString() );
    explicit ReosParameterSlopeWidget( ReosParameterSlope *slope, QWidget *parent = nullptr );

    void setSlope( ReosParameterSlope *slope );
    void updateValue();
    void applyValue();

    static QString type() {return QStringLiteral( "slope" );}

    ReosParameterSlope *slopeParameter() const;

  private:

    QLabel *mLabelSlopeUnit = nullptr;
    int mFactor = 100;
};

class REOSGUI_EXPORT ReosDurationUnitComboBox: public QComboBox
{
    Q_OBJECT
  public:
    ReosDurationUnitComboBox( QWidget *parent = nullptr, ReosDuration::Unit timeUnit = ReosDuration::minute );

    //! Returns the current time unit
    ReosDuration::Unit currentUnit() const;

    //! Sets the current time unit
    void setCurrentUnit( ReosDuration::Unit unit );
};

class REOSGUI_EXPORT ReosParameterDurationWidget: public ReosParameterInLineWidget
{
    Q_OBJECT
  public:
    explicit ReosParameterDurationWidget( QWidget *parent = nullptr, const QString &defaultName = QString() );
    explicit ReosParameterDurationWidget( ReosParameterDuration *duration, QWidget *parent = nullptr );

    void setDuration( ReosParameterDuration *duration );
    void updateValue();
    void applyValue();

    static QString type() {return QStringLiteral( "duration" );}

    ReosParameterDuration *durationParameter() const;

  private:

    ReosDurationUnitComboBox *mUnitCombobox = nullptr;
};

class REOSGUI_EXPORT ReosParameterDateTimeWidget: public ReosParameterWidget
{
    Q_OBJECT
  public:
    explicit ReosParameterDateTimeWidget( QWidget *parent = nullptr, const QString &defaultName = QString() );
    explicit ReosParameterDateTimeWidget( ReosParameterDateTime *dateTime, QWidget *parent = nullptr );

    void setDateTime( ReosParameterDateTime *dateTime );
    void updateValue() override;
    void applyValue() override;
    void setFocusOnEdit() override;

    static QString type() {return QStringLiteral( "date-time" );}

    ReosParameterDateTime *dateTimeParameter() const;

  private:
    QDateTimeEdit *mDateTimeEdit = nullptr;
};

class REOSGUI_EXPORT ReosParameterBooleanWidget : public ReosParameterWidget
{
    Q_OBJECT
  public:
    explicit ReosParameterBooleanWidget( QWidget *parent = nullptr, const QString &defaultName = QString() );
    explicit ReosParameterBooleanWidget( ReosParameterBoolean *booleanParameter, QWidget *parent = nullptr );

    void setBooleanParameter( ReosParameterBoolean *boolean );
    void updateValue() override;
    void applyValue() override;
    void setFocusOnEdit() override;

    static QString type() {return QStringLiteral( "boolean" );}

    ReosParameterBoolean *booleanParameter() const;

  private:
    QCheckBox *mCheckBox = nullptr;
};

// Derived from QTextEdit to have a signal editingFinished when the focus go out
class REOSGUI_EXPORT ReosParameterTextEdit: public QTextEdit
{
    Q_OBJECT
  public:
    ReosParameterTextEdit( QWidget *parent );

  signals:
    void editingFinished();

  protected:
    void focusOutEvent( QFocusEvent *event ) override;
};

class REOSGUI_EXPORT ReosParameterLongStringWidget : public ReosParameterWidget
{
    Q_OBJECT
  public:
    explicit ReosParameterLongStringWidget( QWidget *parent = nullptr, const QString &defaultName = QString() );
    explicit ReosParameterLongStringWidget( ReosParameterLongString *longStringParameter, QWidget *parent = nullptr );

    void setString( ReosParameterLongString *string );
    void updateValue() override;
    void applyValue() override;
    void setFocusOnEdit() override;

    static QString type() {return QStringLiteral( "long-string" );}

    ReosParameterLongString *stringParameter() const;

  private:
    ReosParameterTextEdit *mTextEdit;

};
#endif // REOSPARAMETERWIDGET_H
