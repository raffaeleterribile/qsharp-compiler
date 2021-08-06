#include <cstdlib>

#include "QirRuntimeApi_I.hpp"
#include "QSharpSimApi_I.hpp"

#include "QubitManager.hpp"

#include "Eigen/Dense"

using State = Eigen::VectorXcd;
using Gate = Eigen::Matrix2cd;
using Pauli = Eigen::Matrix2cd;
using Operator = Eigen::MatrixXcd;

namespace Microsoft
{
namespace Quantum
{
    class CSimpleSimulator : public IRuntimeDriver, public IQuantumGateSet
    {
        // Associated qubit manager instance to handle qubit representation.
        QubitManager *qbm;
        short numActiveQubits = 0;

        // State of a qubit is represented by its full 2^n column vector of probability amplitudes.
        // With no qubits allocated, the state starts out as the scalar 1.
        State stateVec = State::Ones(1);

        // To be called by the qubit manager on allocation/deallocation.
        void UpdateState(short qubitIndex, bool remove = false);

        // To be called by quantum gate set operations.
        void ApplyGate(Gate gate, short qubitIndex);
        void ApplyControlledGate(Gate gate, long numControls, Qubit controls[], short targetIndex);

        // Builds a unitary matrix over the state space made of Pauli operators.
        Operator BuildPauliUnitary(long numTargets, PauliId paulis[], Qubit targets[]);

      public:
        CSimpleSimulator(uint32_t userProvidedSeed = 0)
        {
            srand(userProvidedSeed);
        }
        ~CSimpleSimulator() override = default;


        ///
        /// Implementation of IRuntimeDriver
        ///
        void ReleaseResult(Result r) override;

        bool AreEqualResults(Result r1, Result r2) override;

        ResultValue GetResultValue(Result r) override;

        Result UseZero() override;

        Result UseOne() override;

        Qubit AllocateQubit() override;

        void ReleaseQubit(Qubit q) override;

        std::string QubitToString(Qubit q) override;


        ///
        /// Implementation of IQuantumGateSet
        ///
        void X(Qubit q) override;

        void ControlledX(long numControls, Qubit controls[], Qubit target) override;

        void Y(Qubit q) override;

        void ControlledY(long numControls, Qubit controls[], Qubit target) override;

        void Z(Qubit q) override;

        void ControlledZ(long numControls, Qubit controls[], Qubit target) override;

        void H(Qubit q) override;

        void ControlledH(long numControls, Qubit controls[], Qubit target) override;

        void S(Qubit q) override;

        void ControlledS(long numControls, Qubit controls[], Qubit target) override;

        void AdjointS(Qubit q) override;

        void ControlledAdjointS(long numControls, Qubit controls[], Qubit target) override;

        void T(Qubit q) override;

        void ControlledT(long numControls, Qubit controls[], Qubit target) override;

        void AdjointT(Qubit q) override;

        void ControlledAdjointT(long numControls, Qubit controls[], Qubit target) override;

        void R(PauliId axis, Qubit target, double theta) override;

        void ControlledR(long numControls, Qubit controls[], PauliId axis, Qubit target, double theta) override;

        void Exp(long numTargets, PauliId paulis[], Qubit targets[], double theta) override;

        void ControlledExp(long numControls, Qubit controls[], long numTargets, PauliId paulis[], Qubit targets[], double theta) override;

        Result Measure(long numBases, PauliId bases[], long numTargets, Qubit targets[]) override;

    }; // class CSimpleSimulator

} // namespace Quantum
} // namespace Microsoft
