/***************************************************************************
                      reosvertexzspecifier.h
                     --------------------------------------
Date                 : 01-09-2019
Copyright            : (C) 2019 by Vincent Cloarec
email                : vcloarec at gmail dot com / projetreos at gmail dot com
 ******************************************************************************
 *                                                                            *
 *   This program is free software; you can redistribute it and/or modify     *
 *   it under the terms of the GNU General Public License as published by     *
 *   the Free Software Foundation; either version 2 of the License, or        *
 *   (at your option) any later version.                                      *
 *                                                                            *
 *****************************************************************************/

#ifndef REOSVERTEXZSPECIFIER_H
#define REOSVERTEXZSPECIFIER_H

#include <mutex>
#include <list>
#include <iostream>

#include "vertex.h"


#define INVALID_VALUE -999999;

//***********************************************************************************
class ReosVertexZSpecifier
{
public:
    enum class Type {Simple,VertexAndSlope,VertexAndGap,Interpolator};

    ReosVertexZSpecifier(const VertexPointer associatedVertex);
    virtual ~ReosVertexZSpecifier();

    virtual ReosVertexZSpecifier *clone(VertexPointer associatedVertex) const=0;
    double zValue() const;
    virtual void setDirty();
    virtual void hasToBeRemove();

    /////////////////////////////////////////////////////////
    /// \brief surrogateZSpecifier
    /// To use when a linked vertex will be removed to have a surrogate specifier not linked wih the removed vertex
    /// \return a new Z specifier which id associated with mAssociatedVertex. This method supposes that this current specifier will be deleted.
    ///
    virtual std::unique_ptr<ReosVertexZSpecifier> surrogateZSpecifier(VertexPointer=nullptr);

    virtual Type type() const=0;

    struct Data
    {
        std::string type;
        std::vector <double> doubleData;
        std::vector<VertexPointer> otherVertices;
        std::vector<int> verticesIndexes;
    };

    virtual Data data() const;


protected:
    const VertexPointer mAssociatedVertex;
    mutable double mZValue=0;
    mutable bool mDirty=true;
    mutable std::mutex mMutex;

private:
    virtual void calculateZValue() const;

};

class ReosVertexZSpecifierFactory
{
public:
    virtual ~ReosVertexZSpecifierFactory();
    virtual   std::unique_ptr<ReosVertexZSpecifier> createZSpecifier(const VertexPointer associatedVertex) const =0;
    virtual bool IsCompatibleZSpecifier(const VertexPointer associatedVertex) const =0;

    friend class Vertex;
};

//***********************************************************************************

class ReosVertexZSpecifierSimple : public ReosVertexZSpecifier
{
public:
    ReosVertexZSpecifierSimple(const VertexPointer associatedVertex);
    ReosVertexZSpecifierSimple(const VertexPointer associatedVertex,double z);

    ReosVertexZSpecifier *clone(VertexPointer associatedVertex) const override;


    // ReosVertexZSpecifier interface
public:
    Type type() const override {return Type::Simple;}

    Data data() const override;

};

class ReosVertexZSpecifierSimpleFactory:public ReosVertexZSpecifierFactory
{
public:
    ReosVertexZSpecifierSimpleFactory(){}
    ReosVertexZSpecifierSimpleFactory(double zValue);

    void setZValue(double z){mZValue=z;}

    std::unique_ptr<ReosVertexZSpecifier> createZSpecifier(const VertexPointer associatedVertex) const override;
    virtual bool IsCompatibleZSpecifier(const VertexPointer associatedVertex) const override;

private:
    double mZValue=0;

};

//***********************************************************************************

class ReosVertexZSpecifierDependOnOtherVertex : public ReosVertexZSpecifier
{
public:
    ReosVertexZSpecifierDependOnOtherVertex(VertexPointer associatedVertex,VertexPointer otherVertex);
    virtual ~ReosVertexZSpecifierDependOnOtherVertex() override;

    VertexPointer otherVertex() const {return mOtherVertex;}

protected:
    VertexPointer mOtherVertex=nullptr;


    // VertexZSpecifier interface
public:
    std::unique_ptr<ReosVertexZSpecifier> surrogateZSpecifier(VertexPointer vertexRemoved) override;
    void hasToBeRemove() override;
};


class ReosVertexZSpecifierDependOnOtherVertexFactory:public ReosVertexZSpecifierFactory
{
public:
    ReosVertexZSpecifierDependOnOtherVertexFactory();
    ReosVertexZSpecifierDependOnOtherVertexFactory(VertexPointer otherVertex);
    virtual ~ReosVertexZSpecifierDependOnOtherVertexFactory() override;

    void setOtherVertex(VertexPointer otherVertex);
    bool IsCompatibleZSpecifier(const VertexPointer associatedVertex) const override;

    VertexPointer otherVertex() const
    {
        return mOtherVertex;
    }

protected:
    VertexPointer mOtherVertex=nullptr;


private:

};

//***********************************************************************************

class ReosVertexZSpecifierOtherVertexAndSlope : public ReosVertexZSpecifierDependOnOtherVertex
{
public:
    ReosVertexZSpecifierOtherVertexAndSlope(VertexPointer associatedVertex,VertexPointer otherVertex,double slope);
    ReosVertexZSpecifier *clone(VertexPointer associatedVertex) const override;

private:
    double mSlope;

    // ReosVertexZSpecifier interface
public:
    Type type() const override {return Type::VertexAndSlope;}
    Data data() const override;

private:
    void calculateZValue()  const override;
};


class ReosVertexZSpecifierOtherVertexAndSlopeFactory:public ReosVertexZSpecifierDependOnOtherVertexFactory
{
public:
    ReosVertexZSpecifierOtherVertexAndSlopeFactory();
    ReosVertexZSpecifierOtherVertexAndSlopeFactory(VertexPointer otherVertex, double slope);

    void setSlope(double slope);

    std::unique_ptr<ReosVertexZSpecifier> createZSpecifier(const VertexPointer associatedVertex) const override;
private:
    double mSlope=0;

};

//***********************************************************************************

class ReosVertexZSpecifierOtherVertexAndGap : public ReosVertexZSpecifierDependOnOtherVertex
{
public:
    ReosVertexZSpecifierOtherVertexAndGap(VertexPointer associatedVertex,VertexPointer otherVertex,double gap);
    ReosVertexZSpecifier *clone(VertexPointer associatedVertex) const override;

private:
    double mGap;

    // ReosVertexZSpecifier interface
public:
    Type type() const override {return Type::VertexAndGap;}
    Data data() const override;

    void calculateZValue() const override;
};


class ReosVertexZSpecifierOtherVertexAndGapFactory:public ReosVertexZSpecifierDependOnOtherVertexFactory
{
public:
    ReosVertexZSpecifierOtherVertexAndGapFactory();
    ReosVertexZSpecifierOtherVertexAndGapFactory(VertexPointer otherVertex, double gap);

    void setGap(double gap);

    std::unique_ptr<ReosVertexZSpecifier> createZSpecifier(const VertexPointer associatedVertex) const override;

private:
    double mGap=0;

};

//***********************************************************************************

class ReosVertexZSpecifierInterpolation:public ReosVertexZSpecifier
{
public:
    ReosVertexZSpecifierInterpolation(const VertexPointer associatedVertex,
                                  VertexPointer firstVertex, VertexPointer secondVertex,
                                  VertexPointer previous, VertexPointer next, bool withAssociation=true);

    ~ReosVertexZSpecifierInterpolation() override;

    VertexPointer firstExtremity() const;
    VertexPointer secondExtremity() const;

    VertexPointer previousVertexInInterpolation() const;
    VertexPointer nextVertexInInterpolation() const;

    //////////////////////////////////////////////
    /// \brief replace the first extrermity and the extremity of all the next VertexZSpecifierInterpolation
    /// \param vertexExtremity
    ///
    void changeFirstExtremity(VertexPointer vertexExtremity);
    //////////////////////////////////////////////
    /// \brief replace the second extrermity and the extremity of all the previous VertexZSpecifierInterpolation
    /// \param vertexExtremity
    ///
    void changeSecondExtremity(VertexPointer vertexExtremity);

    ///////////////////////////////////////////////////////////////////////////////
    /// \brief replace the first extrermity of only this VertexZSpecifierInterpolation
    /// \param vertexExtremety
    ///
    void setFirstExtremity(VertexPointer vertexExtremity);

    ///////////////////////////////////////////////////////////////////////////////
    /// \brief replace the second extrermity of only this VertexZSpecifierInterpolation
    /// \param vertexExtremety
    ///
    void setSecondExtremity(VertexPointer vertexExtremety);
private:
    VertexPointer mFirstExtremity=nullptr;
    VertexPointer mSecondExtremity=nullptr;
    VertexPointer mPreviousVertexInterpolation=nullptr;
    VertexPointer mNextVertexInterpolation=nullptr;

    // ReosVertexZSpecifier interface
public:
    Type type() const override {return Type::Interpolator;}
    Data data() const override;
    ReosVertexZSpecifier *clone(VertexPointer associatedVertex) const override;
    void hasToBeRemove() override;

private:
    void calculateZValue() const override;
    std::list<VertexPointer> verticesList() const;



    // VertexZSpecifier interface
public:
    std::unique_ptr<ReosVertexZSpecifier> surrogateZSpecifier(VertexPointer vertexRemoved) override;
};


class ReosVertexZSpecifierInterpolationFactory:public ReosVertexZSpecifierFactory
{
public:
    ReosVertexZSpecifierInterpolationFactory();
    ReosVertexZSpecifierInterpolationFactory(VertexPointer firstVertex, VertexPointer secondVertex);

    void setExtremitiesVertices(VertexPointer firstVertex, VertexPointer secondVertex);

    bool IsCompatibleZSpecifier(const VertexPointer associatedVertex) const override;
    std::unique_ptr<ReosVertexZSpecifier> createZSpecifier(const VertexPointer associatedVertex) const override;

protected:

    VertexPointer mFirstExtremity;
    VertexPointer mSecondExtremity;

    mutable std::list<VertexPointer> mAddedVertex;
};

class ReosVertexZSpecifierGeneralFactory:public ReosVertexZSpecifierFactory
{
public:
    void setData(const ReosVertexZSpecifier::Data &data);

private:


    class ReosVertexZSpecifierRawInterpolatorFactory:public ReosVertexZSpecifierInterpolationFactory
    {
    public:
        void setNeighbor(VertexPointer firstExtremityVertex, VertexPointer secondExtremityVertex,
                VertexPointer previousInterpolator, VertexPointer nextInterpolator);

        std::unique_ptr<ReosVertexZSpecifier> createZSpecifier(const VertexPointer associatedVertex) const override;

    private:
        VertexPointer mPreviousInterpolator;
        VertexPointer mNextInterpolator;
    };

    ReosVertexZSpecifierSimpleFactory simpleFactory;
    ReosVertexZSpecifierOtherVertexAndSlopeFactory slopeFactory;
    ReosVertexZSpecifierOtherVertexAndGapFactory gapFactory;
    ReosVertexZSpecifierRawInterpolatorFactory interpolationFactory;

    ReosVertexZSpecifierFactory *currentFactory=nullptr;

    // ReosVertexZSpecifierFactory interface
public:
    std::unique_ptr<ReosVertexZSpecifier> createZSpecifier(const VertexPointer associatedVertex) const override;
    bool IsCompatibleZSpecifier(const VertexPointer associatedVertex) const override;
};


#endif // REOSVERTEXZSPECIFIER_H
