/*******************************************************************************
 * Copyright (c) 2016-2019 Embedded Systems and Applications Group
 * Department of Computer Science, Technische Universitaet Darmstadt,
 * Hochschulstr. 10, 64289 Darmstadt, Germany.
 * <p>
 * All rights reserved.
 * <p>
 * This software is provided free for educational use only.
 * It may not be used for commercial purposes without the
 * prior written permission of the authors.
 ******************************************************************************/
package mavlc.syntax.expression;

import mavlc.errors.InternalCompilerError;
import mavlc.syntax.AstNodeVisitor;
import mavlc.syntax.HasDeclaration;
import mavlc.syntax.SourceLocation;
import mavlc.syntax.statement.Declaration;

import java.util.Objects;

/**
 * AST node representing a reference to a named entity.
 */
public class IdentifierReference extends Expression implements HasDeclaration {
	
	public final String name;
	
	protected Declaration declaration;
	
	/**
	 * @param sourceLocation Location of the node within the containing source file.
	 * @param identifierName The name of the referenced entity.
	 */
	public IdentifierReference(SourceLocation sourceLocation, String identifierName) {
		super(sourceLocation);
		name = identifierName;
	}
	
	/**
	 * Get the declaration side of the referenced entity.
	 *
	 * @return The referenced {@link Declaration}.
	 */
	@Override
	public Declaration getDeclaration() {
		if(declaration == null) throw new InternalCompilerError(this, "Declaration of identifier has not been set");
		return declaration;
	}
	
	/**
	 * Set the declaration side of the referenced entity.
	 *
	 * @param declaration The referenced {@link Declaration}
	 */
	@Override
	public void setDeclaration(Declaration declaration) {
		this.declaration = declaration;
	}
	
	@Override
	public boolean isDeclarationSet() {
		return declaration != null;
	}
	
	@Override
	public <RetTy, ArgTy> RetTy accept(AstNodeVisitor<? extends RetTy, ArgTy> visitor, ArgTy obj) {
		return visitor.visitIdentifierReference(this, obj);
	}
	
	@Override public boolean equals(Object o) {
		if(this == o) return true;
		if(o == null || getClass() != o.getClass()) return false;
		if(!super.equals(o)) return false;
		IdentifierReference that = (IdentifierReference) o;
		return Objects.equals(name, that.name) &&
				Objects.equals(declaration, that.declaration);
	}
	
	@Override public int hashCode() {
		return Objects.hash(super.hashCode(), name);
	}
}
