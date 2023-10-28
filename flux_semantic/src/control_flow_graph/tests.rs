use flux_base::{
    diagnostic::{Dummy, Storage},
    source_file::{SourceElement, SourceFile},
};
use flux_lexical::token_stream::TokenStream;
use flux_syntax::parser::Parser;

use super::LoweringContext;
use crate::error::Error;

const WHILE_STATEMNT: &str = "\
{
    firstFunction();

    while (cond) 
        secondFunction();

    thirdFunction();
}";

/*
entry:
    firstFunction()
    goto header

header:
    condJump cond body exit

body:
    scopePush
    secondFunction()
    scopePop
    goto header

exit:
    thirdFunction()
*/
#[test]
fn while_lowering_test() {
    let mut context = LoweringContext::new();
    let source_file = SourceFile::temp(WHILE_STATEMNT).unwrap();
    let token_stream = TokenStream::tokenize(&source_file, &Dummy);
    let storage: Storage<Error> = Storage::new();

    let mut parser = Parser::new(&token_stream);

    let block = parser.parse_statement(&Dummy).unwrap();

    for statement in block.into_block().unwrap().dissolve().1 {
        context.lower(statement, &storage);
    }

    assert!(storage.as_vec().is_empty());

    // assert entry basic block
    let loop_header_id = {
        assert_eq!(context.control_flow_graph.entry().instructions.len(), 2);

        assert_eq!(
            context.control_flow_graph.entry().instructions[0]
                .as_evalueate()
                .unwrap()
                .span()
                .str(),
            "firstFunction()"
        );

        let loop_header_id = *context.control_flow_graph.entry().instructions[1]
            .as_jump()
            .unwrap();

        loop_header_id
    };

    // assert loop header basic block
    let (loop_body_id, exit_id) = {
        assert_eq!(
            context.control_flow_graph.basic_blocks[loop_header_id]
                .instructions
                .len(),
            1
        );

        let conditional_jump = context.control_flow_graph.basic_blocks[loop_header_id].instructions
            [0]
        .as_conditional_jump()
        .unwrap();

        assert_eq!(conditional_jump.condition.span().str(), "cond");

        (conditional_jump.true_block, conditional_jump.false_block)
    };

    // assert loop body basic block
    {
        assert_eq!(
            context.control_flow_graph.basic_blocks[loop_body_id]
                .instructions
                .len(),
            4
        );

        assert!(
            context.control_flow_graph.basic_blocks[loop_body_id].instructions[0].is_scope_push()
        );

        assert_eq!(
            context.control_flow_graph.basic_blocks[loop_body_id].instructions[1]
                .as_evalueate()
                .unwrap()
                .span()
                .str(),
            "secondFunction()"
        );

        assert!(
            context.control_flow_graph.basic_blocks[loop_body_id].instructions[2].is_scope_pop()
        );

        assert_eq!(
            *context.control_flow_graph.basic_blocks[loop_body_id].instructions[3]
                .as_jump()
                .unwrap(),
            loop_header_id
        );
    }

    // assert exit basic block
    {
        assert_eq!(
            context.control_flow_graph.basic_blocks[exit_id]
                .instructions
                .len(),
            1
        );

        assert_eq!(
            context.control_flow_graph.basic_blocks[exit_id].instructions[0]
                .as_evalueate()
                .unwrap()
                .span()
                .str(),
            "thirdFunction()"
        );
    }
}

const IF_STATEMENT: &str = "\
{
    firstFunction();

    if (cond) 
        secondFunction();

    thridFunction();
}
";

/*
entry:
    firstFunction()
    condJump cond then exit

then:
    scopePush
    secondFunction()
    scopePop
    goto exit

exit:
    thirdFunction()
 */

#[test]
fn if_lowering_test() {
    let mut context = LoweringContext::new();
    let source_file = SourceFile::temp(IF_STATEMENT).unwrap();
    let token_stream = TokenStream::tokenize(&source_file, &Dummy);
    let storage: Storage<Error> = Storage::new();

    let mut parser = Parser::new(&token_stream);

    let block = parser.parse_statement(&Dummy).unwrap();

    for statement in block.into_block().unwrap().dissolve().1 {
        context.lower(statement, &storage);
    }

    assert!(storage.as_vec().is_empty());

    // assert entry basic block
    let (then_id, exit_id) = {
        assert_eq!(context.control_flow_graph.entry().instructions.len(), 2);

        assert_eq!(
            context.control_flow_graph.entry().instructions[0]
                .as_evalueate()
                .unwrap()
                .span()
                .str(),
            "firstFunction()"
        );

        let conditional_jump = context.control_flow_graph.entry().instructions[1]
            .as_conditional_jump()
            .unwrap();

        assert_eq!(conditional_jump.condition.span().str(), "cond");

        (conditional_jump.true_block, conditional_jump.false_block)
    };

    // assert then basic block
    {
        assert_eq!(
            context.control_flow_graph.basic_blocks[then_id]
                .instructions
                .len(),
            4
        );

        assert!(context.control_flow_graph.basic_blocks[then_id].instructions[0].is_scope_push());

        assert_eq!(
            context.control_flow_graph.basic_blocks[then_id].instructions[1]
                .as_evalueate()
                .unwrap()
                .span()
                .str(),
            "secondFunction()"
        );

        assert!(context.control_flow_graph.basic_blocks[then_id].instructions[2].is_scope_pop());

        assert_eq!(
            *context.control_flow_graph.basic_blocks[then_id].instructions[3]
                .as_jump()
                .unwrap(),
            exit_id
        );
    }

    // assert exit basic block
    {
        assert_eq!(
            context.control_flow_graph.basic_blocks[exit_id]
                .instructions
                .len(),
            1
        );

        assert_eq!(
            context.control_flow_graph.basic_blocks[exit_id].instructions[0]
                .as_evalueate()
                .unwrap()
                .span()
                .str(),
            "thridFunction()"
        );
    }
}

const IF_ELSE_STATEMENT: &str = "\
{
    firstFunction();

    if (cond) 
        secondFunction();
    else
        thirdFunction();

    fourthFunction();
}
";

/*
entry:
    firstFunction()
    condJump cond then else

then:
    scopePush
    secondFunction()
    scopePop
    goto exit

else:
    scopePush
    thirdFunction()
    scopePop
    goto exit

exit:
    fourthFunction()
 */

#[test]
fn if_else_lower_test() {
    let mut context = LoweringContext::new();
    let source_file = SourceFile::temp(IF_ELSE_STATEMENT).unwrap();
    let token_stream = TokenStream::tokenize(&source_file, &Dummy);
    let storage: Storage<Error> = Storage::new();

    let mut parser = Parser::new(&token_stream);

    let block = parser.parse_statement(&Dummy).unwrap();

    for statement in block.into_block().unwrap().dissolve().1 {
        context.lower(statement, &storage);
    }

    assert!(storage.as_vec().is_empty());

    // assert entry basic block
    let (then_id, else_id) = {
        assert_eq!(context.control_flow_graph.entry().instructions.len(), 2);

        assert_eq!(
            context.control_flow_graph.entry().instructions[0]
                .as_evalueate()
                .unwrap()
                .span()
                .str(),
            "firstFunction()"
        );

        let conditional_jump = context.control_flow_graph.entry().instructions[1]
            .as_conditional_jump()
            .unwrap();

        assert_eq!(conditional_jump.condition.span().str(), "cond");

        (conditional_jump.true_block, conditional_jump.false_block)
    };

    // assert then basic block
    let then_to_exit_id = {
        assert_eq!(
            context.control_flow_graph.basic_blocks[then_id]
                .instructions
                .len(),
            4
        );

        assert!(context.control_flow_graph.basic_blocks[then_id].instructions[0].is_scope_push());

        assert_eq!(
            context.control_flow_graph.basic_blocks[then_id].instructions[1]
                .as_evalueate()
                .unwrap()
                .span()
                .str(),
            "secondFunction()"
        );

        assert!(context.control_flow_graph.basic_blocks[then_id].instructions[2].is_scope_pop());

        *context.control_flow_graph.basic_blocks[then_id].instructions[3]
            .as_jump()
            .unwrap()
    };

    // assert else basic block
    let else_to_exit_id = {
        assert_eq!(
            context.control_flow_graph.basic_blocks[else_id]
                .instructions
                .len(),
            4
        );

        assert!(context.control_flow_graph.basic_blocks[else_id].instructions[0].is_scope_push());

        assert_eq!(
            context.control_flow_graph.basic_blocks[else_id].instructions[1]
                .as_evalueate()
                .unwrap()
                .span()
                .str(),
            "thirdFunction()"
        );

        assert!(context.control_flow_graph.basic_blocks[else_id].instructions[2].is_scope_pop());

        *context.control_flow_graph.basic_blocks[else_id].instructions[3]
            .as_jump()
            .unwrap()
    };

    assert_eq!(then_to_exit_id, else_to_exit_id);

    // assert exit basic block
    {
        assert_eq!(
            context.control_flow_graph.basic_blocks[then_to_exit_id]
                .instructions
                .len(),
            1
        );

        assert_eq!(
            context.control_flow_graph.basic_blocks[then_to_exit_id].instructions[0]
                .as_evalueate()
                .unwrap()
                .span()
                .str(),
            "fourthFunction()"
        );
    }
}
