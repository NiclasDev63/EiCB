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

import org.xmlunit.diff.Comparison;

import mavlc.errors.NonConstantExpressionError;
import mavlc.syntax.AstNode;
import mavlc.syntax.AstNodeBaseVisitor;
import mavlc.syntax.expression.*;

/* TODO enter group information
 *
 * EiCB group number: ...
 * Names and matriculation numbers of all group members:
 * ...
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
	

	@Override
	public Integer visitBinaryExpression(BinaryExpression binaryExpression, Void __) {
		int left = binaryExpression.leftOperand.accept(this);
		int right = binaryExpression.rightOperand.accept(this);
		if (binaryExpression instanceof And) {
			return left & right;
		} else if (binaryExpression instanceof Or) {
			return left | right;
		} else if (binaryExpression instanceof Addition) {
			return left + right;
		} else if (binaryExpression instanceof Subtraction) {
			return left - right;
		} else if (binaryExpression instanceof Multiplication) {
			return left * right;
		} else if (binaryExpression instanceof Division) {
			return left / right;
		} else if (binaryExpression instanceof Exponentiation) {
			return (int) Math.pow(left, right);
		} else if (binaryExpression instanceof Compare) {
			Compare comp = (Compare) binaryExpression;
			switch (comp.comparator) {
			case LESS:
				return left < right ? 1 : 0;
			case GREATER:
				return left > right ? 1 : 0;
			case LESS_EQUAL:
				return left <= right ? 1 : 0;
			case GREATER_EQUAL:
				return left >= right ? 1 : 0;
			case NOT_EQUAL:
				return left != right ? 1 : 0;
			case EQUAL:
				return left == right ? 1 : 0;
			default:
				throw new RuntimeException("Unsupported comparator: " + comp.comparator);
		}
		} else {
			throw new RuntimeException("Unsupported binary operator: " + binaryExpression);
		}
	}
	

	@Override	
	public Integer visitUnaryExpression(UnaryExpression unaryExpression, Void __) {
		int operand = unaryExpression.operand.accept(this);
		
		if (unaryExpression instanceof UnaryMinus) {
			return -operand;
		} else if (unaryExpression instanceof Not) {
			return operand == 0 ? 1 : 0;
		} else {
			throw new RuntimeException("Unsupported unary operator: " + unaryExpression);
		}
	}
}