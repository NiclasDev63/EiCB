/*******************************************************************************
 * Copyright (c) 2016-2019 Embedded Systems and Applications Group
 * Department of Computer Science, Technische Universitaet Darmstadt,
 * Hochschulstr. 10, 64289 Darmstadt, Germany.
 *
 * All rights reserved.
 *
 * This software is provided free for educational use only.
 * It may not be used for commercial purposes without the
 * prior written permission of the authors.
 ******************************************************************************/
package mavlc.context_analysis;

import java.util.Stack;

import mavlc.syntax.statement.Declaration;

/* TODO enter group information
 *
 * EiCB group number: ...
 * Names and matriculation numbers of all group members:
 * ...
 */

/**
 * A table for identifiers used inside a function.
 */
public class IdentificationTable {

	private Stack<Scope> scopes = new Stack<Scope>();
	
	/**
	 * Declares the given identifier in the current scope.
	 *
	 * @param name the identifier to declare
	 * @param declaration the reference to the identifier's declaration site
	 */
	public void addIdentifier(String name, Declaration declaration) {
		this.scopes.peek().addIdentifier(name, declaration);
	}
	
	/**
	 * Looks up the innermost declaration of the given identifier.
	 *
	 * @param name the identifier to look up
	 * @return the identifier's innermost declaration site
	 */
	public Declaration getDeclaration(String name) {
		return this.scopes.peek().getDeclaration(name);
	}
	
	/**
	 * Opens a new scope.
	 */
	public void openNewScope() {
        if (this.scopes.empty()) {
            this.scopes.push(new Scope(null));
        } else { 
            this.scopes.push(new Scope(this.scopes.peek()));        
        }
	}
	
	/**
	 * Closes the current scope.
	 */
	public void closeCurrentScope() {
		this.scopes.pop(); 
	}
}
