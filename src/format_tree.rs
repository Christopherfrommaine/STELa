pub fn format_tree(input: &str) -> String {
    let mut result = String::new();
    let mut depth = 0;
    let mut chars = input.chars().peekable();
    let mut last_char_was_newline = false;

    while let Some(c) = chars.next() {
        match c {
            '{' | '[' | '(' => {
                result.push(c);
                result.push('\n');
                depth += 1;
                result.push_str(&"  ".repeat(depth));
                last_char_was_newline = true;
            }
            '}' | ']' | ')' => {
                result.push('\n');
                depth -= 1;
                result.push_str(&"  ".repeat(depth));
                result.push(c);
                last_char_was_newline = true;
            }
            ',' => {
                result.push(',');
                result.push('\n');
                result.push_str(&"  ".repeat(depth));
                last_char_was_newline = true;
            }
            ':' => {
                result.push(':');
                if chars.peek().map_or(false, |c| !c.is_whitespace()) {
                    result.push(' ');
                }
                last_char_was_newline = false;
            }
            ' ' | '\n' | '\t' => {
                if !last_char_was_newline {
                    result.push(' ');
                }
                last_char_was_newline = false;
            }
            _ => {
                result.push(c);
                last_char_was_newline = false;
            }
        }
    }

    result
}
