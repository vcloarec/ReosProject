#ifndef REOSVERTEXZSPECIFIER_H
#define REOSVERTEXZSPECIFIER_H

#include "reosmesh.h"

//***********************************************************************************
class ReosVertexZSpecifier
{
public:
    ReosVertexZSpecifier(const VertexPointer associatedVertex);
    virtual ~ReosVertexZSpecifier();

    virtual ReosVertexZSpecifier *clone(VertexPointer associatedVertex) const;
    double zValue() const;
    virtual void setDirty();
    virtual void hasToBeRemove() {}

    /////////////////////////////////////////////////////////
    /// \brief surrogateZSpecifier
    /// To use when a linked vertex will be removed to have a surrogate specifier not linked wih the removed vertex
    /// \return a new Z specifier which id associated with mAssociatedVertex. This method supposes that this current specifier will be deleted.
    ///
    virtual std::unique_ptr<ReosVertexZSpecifier> surrogateZSpecifier(VertexPointer=nullptr)
    {
        return std::unique_ptr<ReosVertexZSpecifier>(mAssociatedVertex->releaseZSpecifier());
    }

protected:
    const VertexPointer mAssociatedVertex;
    mutable double mZValue=0;
    mutable bool mDirty=true;
    mutable std::mutex mMutex;

private:
    virtual void calculateZValue() const {}

};

class ReosVertexZSpecifierFactory
{
public:
    virtual ~ReosVertexZSpecifierFactory() {}
private:
    virtual   std::unique_ptr<ReosVertexZSpecifier> createZSpecifier(const VertexPointer associatedVertex) const =0;

    friend class Vertex;
};

//***********************************************************************************

class ReosVertexZSpecifierSimple : public ReosVertexZSpecifier
{
public:
    ReosVertexZSpecifierSimple(const VertexPointer associatedVertex);
    ReosVertexZSpecifierSimple(const VertexPointer associatedVertex,double z);

    ReosVertexZSpecifier *clone(VertexPointer associatedVertex) const override;

};

class ReosVertexZSpecifierSimpleFactory:public ReosVertexZSpecifierFactory
{
public:
    ReosVertexZSpecifierSimpleFactory(){}
    ReosVertexZSpecifierSimpleFactory(double zValue);

    void setZValue(double z){mZValue=z;}

private:
    double mZValue=0;

    std::unique_ptr<ReosVertexZSpecifier> createZSpecifier(const VertexPointer associatedVertex) const override;
};

//***********************************************************************************

class ReosVertexZSpecifierDependOnOtherVertex : public ReosVertexZSpecifier
{
public:
    ReosVertexZSpecifierDependOnOtherVertex(VertexPointer associatedVertex,VertexPointer otherVertex);
    virtual ~ReosVertexZSpecifierDependOnOtherVertex()
    {
        if (mOtherVertex)
            mOtherVertex->removeDependentVertex(mAssociatedVertex);
    }
protected:
    VertexPointer mOtherVertex=nullptr;


    // VertexZSpecifier interface
public:
    std::unique_ptr<ReosVertexZSpecifier> surrogateZSpecifier(VertexPointer vertexRemoved) override
    {
        if (vertexRemoved==mOtherVertex)
        {
            hasToBeRemove();
            return std::make_unique<ReosVertexZSpecifierSimple>(mAssociatedVertex,mZValue);
        }
        else
            return ReosVertexZSpecifier::surrogateZSpecifier(vertexRemoved);
    }
};


class ReosVertexZSpecifierDependOnOtherVertexFactory:public ReosVertexZSpecifierFactory
{
public:
    ReosVertexZSpecifierDependOnOtherVertexFactory(){}
    ReosVertexZSpecifierDependOnOtherVertexFactory(VertexPointer otherVertex):ReosVertexZSpecifierFactory(),mOtherVertex(otherVertex)
    {

    }
    virtual ~ReosVertexZSpecifierDependOnOtherVertexFactory() {}

    void setOtherVertex(VertexPointer otherVertex)
    {
        mOtherVertex=otherVertex;
    }

protected:
    VertexPointer mOtherVertex=nullptr;

};

//***********************************************************************************

class ReosVertexZSpecifierOtherVertexAndSlope : public ReosVertexZSpecifierDependOnOtherVertex
{
public:
    ReosVertexZSpecifierOtherVertexAndSlope(VertexPointer associatedVertex,VertexPointer otherVertex,double slope);
    ReosVertexZSpecifier *clone(VertexPointer associatedVertex) const override;

private:
    double mSlope;


    // VertexZSpecifier interface
private:
    void calculateZValue()  const override;
};


class ReosVertexZSpecifierOtherVertexAndSlopeFactory:public ReosVertexZSpecifierDependOnOtherVertexFactory
{
public:
    ReosVertexZSpecifierOtherVertexAndSlopeFactory(){}
    ReosVertexZSpecifierOtherVertexAndSlopeFactory(VertexPointer otherVertex, double slope):
        ReosVertexZSpecifierDependOnOtherVertexFactory(otherVertex),mSlope(slope)
    {}

    std::unique_ptr<ReosVertexZSpecifier> createZSpecifier(const VertexPointer associatedVertex) const override
    {
        return std::make_unique<ReosVertexZSpecifierOtherVertexAndSlope>(associatedVertex,mOtherVertex,mSlope);
    }

    void setSlope(double slope)
    {
        mSlope=slope;
    }

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

    // VertexZSpecifier interface
private:
    void calculateZValue() const override
    {
        if(mOtherVertex)
            mZValue=mOtherVertex->z()+mGap;
        else
            mZValue=INVALID_VALUE;
    }
};


class ReosVertexZSpecifierOtherVertexAndGapFactory:public ReosVertexZSpecifierDependOnOtherVertexFactory
{
public:
    ReosVertexZSpecifierOtherVertexAndGapFactory(){}
    ReosVertexZSpecifierOtherVertexAndGapFactory(VertexPointer otherVertex, double gap):
        ReosVertexZSpecifierDependOnOtherVertexFactory(otherVertex),mGap(gap)
    {}

    std::unique_ptr<ReosVertexZSpecifier> createZSpecifier(const VertexPointer associatedVertex) const override
    {
        return std::make_unique<ReosVertexZSpecifierOtherVertexAndGap>(associatedVertex,mOtherVertex,mGap);
    }

    void setGap(double gap);

private:
    double mGap=0;

};

//***********************************************************************************

class ReosVertexZSpecifierInterpolation:public ReosVertexZSpecifier
{
public:
    ReosVertexZSpecifierInterpolation(const VertexPointer associatedVertex,
                                  VertexPointer firstVertex, VertexPointer secondVertex,
                                  ReosVertexZSpecifierInterpolation *previous, ReosVertexZSpecifierInterpolation *next);

    ~ReosVertexZSpecifierInterpolation() override;

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
    ReosVertexZSpecifierInterpolation *mPreviousSpecifierInterpolation=nullptr;
    ReosVertexZSpecifierInterpolation *mNextSpecifierInterpolation=nullptr;

    // VertexZSpecifier interface
public:
    ReosVertexZSpecifier *clone(VertexPointer associatedVertex) const override;
    void hasToBeRemove() override;
private:
    void calculateZValue() const override;
    std::list<VertexPointer> verticesList() const;



    // VertexZSpecifier interface
public:
    std::unique_ptr<ReosVertexZSpecifier> surrogateZSpecifier(VertexPointer vertexRemoved) override
    {
        if (vertexRemoved==mFirstExtremity || vertexRemoved==mSecondExtremity)
        {
            hasToBeRemove();
            return std::make_unique<ReosVertexZSpecifierSimple>(mAssociatedVertex,mZValue);
        }
        else
            return ReosVertexZSpecifier::surrogateZSpecifier(vertexRemoved);
    }
};


class ReosVertexZSpecifierInterpolationFactory:public ReosVertexZSpecifierFactory
{
public:
    ReosVertexZSpecifierInterpolationFactory();
    ReosVertexZSpecifierInterpolationFactory(VertexPointer firstVertex, VertexPointer secondVertex);
    void setExtremitiesVertices(VertexPointer firstVertex, VertexPointer secondVertex);


private:
    std::unique_ptr<ReosVertexZSpecifier> createZSpecifier(const VertexPointer associatedVertex) const override;

    VertexPointer mFirstExtremity;
    VertexPointer mSecondExtremity;

    mutable std::list<ReosVertexZSpecifierInterpolation*> mAddedSpecifier;
};


#endif // REOSVERTEXZSPECIFIER_H
