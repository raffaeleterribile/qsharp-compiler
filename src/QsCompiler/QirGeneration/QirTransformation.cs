﻿// Copyright (c) Microsoft Corporation. All rights reserved.
// Licensed under the MIT License.

using System.IO;
using Microsoft.Quantum.QsCompiler.SyntaxTree;
using Microsoft.Quantum.QsCompiler.Transformations.Core;

namespace Microsoft.Quantum.QsCompiler.QirGenerator
{
    public class QirTransformation : SyntaxTreeTransformation<GenerationContext>
    {
        public readonly Configuration Config;

        /// <summary>
        /// The compilation unit for which QIR is generated.
        /// </summary>
        public readonly QsCompilation Compilation;

        public readonly FunctionLibrary RuntimeLibrary;

        public readonly FunctionLibrary QuantumInstructionSet;

        public QirTransformation(QsCompilation compilation, Configuration config)
        : base(new GenerationContext(compilation.Namespaces, config), TransformationOptions.NoRebuild)
        {
            this.Config = config;
            this.Compilation = compilation;

            this.SharedState.SetTransformation(this, out this.RuntimeLibrary, out this.QuantumInstructionSet);
            this.SharedState.InitializeRuntimeLibrary();
            this.SharedState.RegisterQuantumInstructionSet();

            this.Namespaces = new QirNamespaceTransformation(this, TransformationOptions.NoRebuild);
            this.StatementKinds = new QirStatementKindTransformation(this, TransformationOptions.NoRebuild);
            this.Expressions = new QirExpressionTransformation(this, TransformationOptions.NoRebuild);
            this.ExpressionKinds = new QirExpressionKindTransformation(this, TransformationOptions.NoRebuild);
            this.Types = new QirTypeTransformation(this, TransformationOptions.NoRebuild);
        }

        public void Apply()
        {
            foreach (var ns in this.Compilation.Namespaces)
            {
                this.Namespaces.OnNamespace(ns);
            }

            foreach (var epName in this.Compilation.EntryPoints)
            {
                this.SharedState.GenerateEntryPoint(epName);
            }
        }

        /// <summary>
        /// Writes the current content to the output file.
        /// </summary>
        public void Emit() => this.SharedState.Emit();
    }
}
