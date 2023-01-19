class ParallelCalcError : public ParallelLoopBody
{
public:
    virtual void operator()(const Range& range) const CV_OVERRIDE
    {
        for (int i = range.start; i < range.end; i++)
        {

        }

    };
};
