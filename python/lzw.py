def compress(uncompressed):
    # Build the dictionary.
    dict_size = 256
    dictionary = {chr(i): i for i in range(dict_size)}

    w = ""
    result = []
    for c in uncompressed:
        wc = w + c
        if wc in dictionary:
            w = wc
        else:
            result.append(dictionary[w])
            # Add wc to the dictionary.
            dictionary[wc] = dict_size
            dict_size += 1
            w = c

    # Output the code for w.
    if w:
        result.append(dictionary[w])
    return result

def decompress(compressed):
    # Build the dictionary.
    dict_size = 256
    dictionary = {i: chr(i) for i in range(dict_size)}

    w = chr(compressed.pop(0))
    result = w
    for k in compressed:
        if k in dictionary:
            entry = dictionary[k]
        elif k == dict_size:
            entry = w + w[0]
        else:
            raise ValueError('Bad compressed k: %s' % k)
        result += entry

        # Add w+entry[0] to the dictionary.
        dictionary[dict_size] = w + entry[0]
        dict_size += 1

        w = entry
    return result

# Example usage
half = "ABABABABABABABABABABABABABABABC"
original = half + half
print("Original:", original)

compressed = compress(original)
print("Compressed:", compressed)

decompressed = decompress(compressed)
print("Decompressed:", decompressed)

print("Compression successful:", original == decompressed)

# Calculate compression ratio
original_size = len(original) * 8  # Assuming 8 bits per character
compressed_size = len(compressed) * 16  # Assuming 16 bits per code
compression_ratio = (1 - compressed_size / original_size) * 100
print(f"Compression ratio: {compression_ratio:.2f}%")
