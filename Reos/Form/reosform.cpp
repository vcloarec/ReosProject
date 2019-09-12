/***************************************************************************
                      reosform.cpp
                     --------------------------------------
Date                 : 31-03-2019
Copyright            : (C) 2019 by Vincent Cloarec
email                :   projetreos@gmail.com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reosform.h"


const QList<ReosFormObject *> &ReosForm::getFormObjectList() const
{
    return attachedFormObjects;
}

void ReosForm::addForm(ReosForm *childForm)
{
    attachedFormObjects.append(childForm);
}

void ReosForm::addParamater(ReosFormParameter *param)
{
    connect(param,&ReosFormParameter::valueEdited,this,&ReosForm::parameterEdit);
    attachedFormObjects.append(param);
}

void ReosForm::addSeparator()
{
    separatorsPosition.append(attachedFormObjects.count());
}

bool ReosForm::isSperatorPresent(int i) const
{
    return  separatorsPosition.contains(i);
}

void ReosForm::parameterEdit()
{
    emit parameterEdited();
}

QWidget *ReosForm::getWidget()
{
    return new ReosFormWidget(this);
}

ReosFormParameter::ReosFormParameter(ReosForm *parent):ReosFormObject (parent)
{
    if (parent)
        parent->addParamater(this);
}

QWidget *ReosFormParameter::getWidget()
{
    ReosFormParameterEditor *editor=makeEdit();
    connect(editor,&ReosFormParameterEditor::askForCalculate,this,&ReosFormParameter::valueAsked);

    return editor;
}


QString ReosFormParameterSimple::getName() const
{
    return name;
}

void ReosFormParameterSimple::setName(const QString &value)
{
    name = value;
}

void ReosFormParameterSimple::linkEditableWithBool(ReosFormParameterSimpleBool *boolParameter, bool editableIfTrue)
{
    if (editableIfTrue)
        connect(boolParameter,&ReosFormParameterSimpleBool::valueChanged,this,&ReosFormParameterSimple::setEditable);
    else {
        connect(boolParameter,&ReosFormParameterSimpleBool::valueChanged,this,&ReosFormParameterSimple::setUneditable);
    }
}

void ReosFormParameterSimple::linkEnableWithBool(ReosFormParameterSimpleBool *boolParameter, bool enableIfTrue)
{
    if (enableIfTrue)
        connect(boolParameter,&ReosFormParameterSimpleBool::valueChanged,this,&ReosFormParameterSimple::enableEditor);
    else {
        connect(boolParameter,&ReosFormParameterSimpleBool::valueChanged,this,&ReosFormParameterSimple::disableEditor);
    }
}


double ReosFormParameterSimpleDouble::getValue() const
{
    return value;
}

void ReosFormParameterSimpleDouble::setValue(double v)
{
    value = v;
    emit valueChanged(getStringValue());

}

QString ReosFormParameterSimpleDouble::getUnit() const
{
    return unit;
}

void ReosFormParameterSimpleDouble::setUnit(const QString &value)
{
    unit = value;
}

double ReosFormParameterSimpleDouble::getNoData() const
{
    return noData;
}

void ReosFormParameterSimpleDouble::setNoData(double value)
{
    noData = value;
}

QString ReosFormParameterSimpleString::getValue() const
{
    return value;
}

void ReosFormParameterSimpleString::setValue(const QString &v)
{
    value = v;
    emit valueChanged(value);
}

bool ReosFormParameterSimpleBool::getValue() const
{
    return value;
}

void ReosFormParameterSimpleBool::setValue(bool v)
{
    value = v;
    emit valueChanged(value);
}

ReosArea ReosFormParameterSimpleArea::getValue() const
{
    return value;
}

void ReosFormParameterSimpleArea::setValue(const ReosArea &v)
{
    value = v;
    emit valueChanged(getStringValue());
    emit unitChange(value.unit());

}

ReosDuration ReosFormParameterSimpleDuration::getValue() const
{
    return value;
}

void ReosFormParameterSimpleDuration::setValue(const ReosDuration &v)
{
    value=v;
    emit valueChanged(getStringValue());
}
