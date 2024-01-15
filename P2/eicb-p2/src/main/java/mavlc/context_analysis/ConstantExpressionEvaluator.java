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

import mavlc.errors.NonConstantExpressionError;
import mavlc.syntax.AstNode;
import mavlc.syntax.AstNodeBaseVisitor;
import mavlc.syntax.expression.*;

/* 
 * EiCB group number: 35
 * Names and matriculation numbers of all group members:
 * Matias Heredia Novillo 2371009, Anastasia Paschalidou 2368876, Niclas Gregor 2637756
 */

public class ConstantExpressionEvaluator extends AstNodeBaseVisitor<Integer, Void> {
	@Override
	protected Integer defaultOperation(AstNode node, Void obj) {
		if(node instanceof Expression) {
			throw new NonConstantExpressionError((Expression) node);
		} else {
			throw new RuntimeException("Internal compiler error: should not try to constant-evaluate non-expressions");
		}
	}
	
	@Override
	public Integer visitIntValue(IntValue intValue, Void __) {
		return intValue.value;
	}

	public Integer visitExponentiation(Exponentiation exponentiation, Void __) {
		int left = exponentiation.leftOperand.accept(this);
		int right = exponentiation.rightOperand.accept(this);
		return (int) Math.pow(left, right);
	}

	public Integer visitAddition(Addition addition, Void __) {
		int left = addition.leftOperand.accept(this);
		int right = addition.rightOperand.accept(this);
		return left + right;
	}

	public Integer visitSubtraction(Subtraction subtraction, Void __) {
		int left = subtraction.leftOperand.accept(this);
		int right = subtraction.rightOperand.accept(this);
		return left - right;
	}

	public Integer visitMultiplication(Multiplication multiplication, Void __) {
		int left = multiplication.leftOperand.accept(this);
		int right = multiplication.rightOperand.accept(this);
		return left * right;
	}

	public Integer visitDivision(Division division, Void __) {
		int left = division.leftOperand.accept(this);
		int right = division.rightOperand.accept(this);
		return (int) left / right;
	}

	public Integer visitUnaryMinus(UnaryMinus unaryMinus, Void __) {
		int operand = unaryMinus.operand.accept(this);
		return -operand;
	}
}