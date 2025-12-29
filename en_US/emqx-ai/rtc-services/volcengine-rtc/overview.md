# Volcano Engine Speech Services

Volcano Engine Real-Time Conversational AI provides core capabilities such as RTC audio/video transmission, ASR speech recognition, and TTS speech synthesis. Developers can integrate their own AI backends through the CustomLLM mode to build voice-driven intelligent interactions.

## What Is Volcano Engine Speech Services

Volcano Engine Real-Time Conversational AI is an end-to-end voice interaction solution that enables intelligent agents to “hear, speak, see, and reason.” It is suitable for scenarios such as AI assistants, AI customer service, AI companionship, AI spoken-language learning, and intelligent hardware.

### Core Components

#### RTC (Real-Time Audio and Video)

Responsible for audio and video transmission between clients and the cloud.

- Based on the WebRTC protocol, supporting mainstream browsers
- Multi-platform SDKs: Web (`@volcengine/rtc`), iOS, Android, Windows, Linux, macOS
- Built-in AI noise suppression (AI-ANS) to filter environmental noise
- Binary message channel for transmitting structured data such as subtitles and status
- Strong resilience to weak network conditions, ensuring reliable transmission in complex environments

#### ASR (Automatic Speech Recognition)

Converts user speech into text in real time.

- Streaming recognition with real-time transcription
- Supports multiple languages, including Chinese, English, Japanese, and Spanish
- Supports hotword configuration to improve recognition accuracy for domain-specific terms
- Frame-level Voice Activity Detection (VAD) for accurate speech start and end detection

#### TTS (Text-to-Speech)

Converts AI-generated text responses into natural-sounding speech.

- Streaming synthesis with low latency
- Multiple voice options (male, female, and different styles)
- Supports adjustment of speech rate, pitch, and volume
- Supports emotional synthesis (e.g., happy, calm)

#### LLM (Large Language Models)

Handles user intent understanding and response generation, with two integration modes:

**Volcano Ark (ArkV3)**

Uses large language models hosted by Volcano Engine, ready to use out of the box.

- Supports multiple models such as Doubao, Claude, and GLM
- No additional service deployment required
- Automatic cloud scaling

**CustomLLM (Custom Backend)**

Volcano Engine invokes the developer’s custom service to obtain LLM responses.

- Can integrate with any LLM (OpenAI, Qwen, local models, etc.)
- Full control over conversation logic
- Supports agent architectures and tool invocation
- Can integrate private knowledge bases

The EMQX MCP AI voice assistant uses the CustomLLM mode to enable MCP tool invocation.

## Extended Capabilities

Volcano Engine Speech Services also provide the following extended features:

| Capability               | Description                                                  |
| ------------------------ | ------------------------------------------------------------ |
| Intelligent Interruption | Full-duplex communication; users can interrupt the AI at any time for more natural interaction |
| Visual Understanding     | Supports image and video input, enabling AI to “see” and understand visual content |
| Function Calling         | Allows the LLM to identify user intent and invoke external functions |
| MCP Protocol Support     | Standardized access to external tool ecosystems              |
| Real-Time Subtitles      | Returns ASR results and LLM responses in real time           |
| Context Management       | Supports short-term and long-term memory (via vector databases) |

For detailed feature descriptions, see the
 [Volcano Engine Real-Time Conversational AI Documentation](https://www.volcengine.com/docs/6348/1310537).

## Pricing

Volcano Engine Speech Services are billed based on usage. Each billing item includes a free trial quota. For details, see
 [Conversational AI Real-Time Pricing](https://www.volcengine.com/docs/6348/1392584).

## Related Resources

- [Volcano Engine Real-Time Conversational AI Documentation](https://www.volcengine.com/docs/6348/1310537)
